package com.wavesplatform.api.http.assets

import java.util.concurrent._

import akka.http.scaladsl.common.EntityStreamingSupport
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import cats.syntax.either._
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi}
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.assets.AssetsApiRoute.DistributionParams
import com.wavesplatform.api.http.requests._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.{AssetDescription, Blockchain}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TransactionFactory
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import io.swagger.annotations._
import javax.ws.rs.Path
import monix.execution.Scheduler
import monix.reactive.Observable
import play.api.libs.json._

import scala.concurrent.Future

@Path("/assets")
@Api(value = "assets")
case class AssetsApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    utxPoolSynchronizer: UtxPoolSynchronizer,
    blockchain: Blockchain,
    time: Time,
    commonAccountApi: CommonAccountsApi,
    commonAssetsApi: CommonAssetsApi
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute {

  private[this] val distributionTaskScheduler = {
    val executor = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable](AssetsApiRoute.MAX_DISTRIBUTION_TASKS))
    Scheduler(executor)
  }

  private def deprecatedRoute: Route =
    (path("transfer") & withAuth) {
      broadcast[TransferRequest](TransactionFactory.transferAsset(_, wallet, time))
    } ~ (path("masstransfer") & withAuth) {
      broadcast[MassTransferRequest](TransactionFactory.massTransferAsset(_, wallet, time))
    } ~ (path("issue") & withAuth) {
      broadcast[IssueRequest](TransactionFactory.issue(_, wallet, time))
    } ~ (path("reissue") & withAuth) {
      broadcast[ReissueRequest](TransactionFactory.reissue(_, wallet, time))
    } ~ (path("burn") & withAuth) {
      broadcast[BurnRequest](TransactionFactory.burn(_, wallet, time))
    } ~ (path("sponsor") & withAuth) {
      broadcast[SponsorFeeRequest](TransactionFactory.sponsor(_, wallet, time))
    } ~ (path("order") & withAuth)(jsonPost[Order] { order =>
      wallet.privateKeyAccount(order.senderPublicKey).map(pk => Order.sign(order, pk))
    }) ~ pathPrefix("broadcast")(
      path("issue")(broadcast[IssueRequest](_.toTx)) ~
        path("reissue")(broadcast[ReissueRequest](_.toTx)) ~
        path("burn")(broadcast[BurnRequest](_.toTx)) ~
        path("exchange")(broadcast[ExchangeRequest](_.toTx)) ~
        path("transfer")(broadcast[TransferRequest](_.toTx))
    )

  override lazy val route: Route =
    pathPrefix("assets") {
      get {
        pathPrefix("balance") {
          pathPrefix(AddrSegment) { address =>
            pathEndOrSingleSlash(balances(address)) ~
              path(AssetId)(balance(address, _))
          }
        } ~ pathPrefix("details") {
          (pathEndOrSingleSlash & parameters('id.*, 'full.as[Boolean] ? false)) { (ids, full) =>
            multipleDetailsGet(ids, full)
          } ~ (path(AssetId) & parameter('full.as[Boolean] ? false)) { (assetId, full) =>
            singleDetails(assetId, full)
          }
        } ~ (path("nft" / AddrSegment / "limit" / IntNumber) & parameter('after.as[String].?)) { (address, limit, maybeAfter) =>
          nft(address, limit, maybeAfter)
        } ~ pathPrefix(AssetId / "distribution") { assetId =>
          pathEndOrSingleSlash(balanceDistribution(assetId)) ~
            (path(IntNumber / "limit" / IntNumber) & parameter('after.?)) { (height, limit, maybeAfter) =>
              balanceDistributionAtHeight(assetId, height, limit, maybeAfter)
            }
        }
      } ~ post {
        deprecatedRoute
      }
    }

  @Path("/balance/{address}")
  @ApiOperation(value = "Account's balance", notes = "Account's balances for all assets", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def balances(address: Address): Route = extractScheduler { implicit s =>
    implicit val jsonStreamingSupport: EntityStreamingSupport = jsonStream(s"""{"address":"$address","balances":[""", ",", "]}")
    complete(
      Source.fromPublisher(
        commonAccountApi
          .portfolio(address)
          .flatMap {
            case (assetId, balance) =>
              Observable.fromIterable(commonAssetsApi.fullInfo(assetId).map {
                case CommonAssetsApi.AssetInfo(assetInfo, issueTransaction, sponsorBalance) =>
                  Json.obj(
                    "assetId"    -> assetId.id.toString,
                    "balance"    -> balance,
                    "reissuable" -> assetInfo.reissuable,
                    "minSponsoredAssetFee" -> (assetInfo.sponsorship match {
                      case 0           => JsNull
                      case sponsorship => JsNumber(sponsorship)
                    }),
                    "sponsorBalance"   -> sponsorBalance,
                    "quantity"         -> JsNumber(BigDecimal(assetInfo.totalVolume)),
                    "issueTransaction" -> issueTransaction.json()
                  )
              })

          }
          .toReactivePublisher
      )
    )
  }

  @Path("/balance/{address}/{assetId}")
  @ApiOperation(value = "Asset's balance", notes = "Account's balance by given asset", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path")
    )
  )
  def balance(address: Address, assetId: IssuedAsset): Route = complete(balanceJson(address, assetId))

  private def balanceDistribution(assetId: IssuedAsset, height: Int, limit: Int, after: Option[Address])(f: List[(Address, Long)] => JsValue) =
    complete {
      try {
        commonAssetsApi
          .assetDistribution(assetId, height, after)
          .take(limit)
          .toListL
          .map(f)
          .runAsyncLogErr(distributionTaskScheduler)
      } catch {
        case _: RejectedExecutionException =>
          val errMsg = CustomValidationError("Asset distribution currently unavailable, try again later")
          Future.successful(errMsg.json: ToResponseMarshallable)
      }
    }

  def balanceDistribution(assetId: IssuedAsset): Route =
    balanceDistribution(assetId, blockchain.height, settings.distributionAddressLimit, None) { l =>
      Json.toJson(l.map { case (a, b) => a.stringRepr -> b }.toMap)
    }

  @Path("/{assetId}/distribution/{height}/limit/{limit}")
  @ApiOperation(
    value = "Asset balance distribution at height",
    notes = "Asset balance distribution by account at specified height",
    httpMethod = "GET"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "height", value = "Height", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "limit", value = "Number of addresses to be returned", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "after", value = "address to paginate after", required = false, dataType = "string", paramType = "query")
    )
  )
  def balanceDistributionAtHeight(assetId: IssuedAsset, heightParam: Int, limitParam: Int, afterParam: Option[String]): Route = {
    val paramsEi: Either[ValidationError, DistributionParams] =
      AssetsApiRoute
        .validateDistributionParams(blockchain, heightParam, limitParam, settings.distributionAddressLimit, afterParam)

    paramsEi match {
      case Right((height, limit, after)) =>
        balanceDistribution(assetId, height, limit, after) { l =>
          Json.obj(
            "hasNext"  -> (l.length == limit),
            "lastItem" -> l.lastOption.map(_._1),
            "items"    -> Json.toJson(l.map { case (a, b) => a.stringRepr -> b }.toMap)
          )
        }
      case Left(error) => complete(error)
    }
  }

  def details: Route = pathPrefix("details")(singleDetails ~ multipleDetails)

  @Path("/details/{assetId}")
  @ApiOperation(value = "Information about an asset", notes = "Provides detailed information about given asset", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "ID of the asset", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "full", value = "false", required = false, dataType = "boolean", paramType = "query")
    )
  )
  def singleDetails(assetId: IssuedAsset, full: Boolean): Route = complete(assetDetails(assetId, full))

  def multipleDetails: Route = pathEndOrSingleSlash(multipleDetailsGet ~ multipleDetailsPost)

  @Path("/details")
  @ApiOperation(value = "Information about assets", notes = "Provides detailed information about given assets", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "id", value = "IDs of the asset", required = true, dataType = "string", paramType = "query"),
      new ApiImplicitParam(name = "full", value = "false", required = false, dataType = "boolean", paramType = "query")
    )
  )
  def multipleDetailsGet(ids: Seq[ByteStr], full: Boolean): Route =
    complete(ids.toList.map(id => assetDetails(IssuedAsset(id), full).fold(_.json, identity)))

  @Path("/details")
  @ApiOperation(value = "Information about assets", notes = "Provides detailed information about given assets", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "json",
        value = "IDs of the asset",
        required = true,
        dataType = "string",
        paramType = "body",
        example = """{"ids": ["some1", "some2"]}"""
      ),
      new ApiImplicitParam(name = "full", value = "false", required = false, dataType = "boolean", paramType = "query")
    )
  )
  def multipleDetailsPost(full: Boolean): Route =
      complete(entity(as[JsObject]) { jsv =>
        (jsv \ "ids").validate[List[ByteStr]] match {
          case JsSuccess(ids, _) => ids.map(id => assetDetails(IssuedAsset(id), full.getOrElse(false)).fold(_.json, identity))
          case JsError(err)      => WrongJson(errors = err)
        }
      })

  @Path("/nft/{address}/limit/{limit}")
  @ApiOperation(value = "NFTs", notes = "Account's NFTs balance", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "limit", value = "Number of tokens to be returned", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "after", value = "Id of token to paginate after", required = false, dataType = "string", paramType = "query")
    )
  )
  def nft(address: Address, limit: Int, maybeAfter: Option[String]): Route = {
    val after = maybeAfter.map(s => IssuedAsset(ByteStr.decodeBase58(s).getOrElse(throw ApiException(InvalidAssetId))))
    if (limit > settings.transactionsByAddressLimit) complete(TooBigArrayAllocation)
    else
      extractScheduler { implicit sc =>
        complete {
          implicit val j: EntityStreamingSupport = EntityStreamingSupport.json()
          Source.fromPublisher(
            commonAccountApi
              .nftPortfolio(address, after)
              .flatMap {
                case (assetId, assetDesc) =>
                  Observable.fromEither(
                    AssetsApiRoute
                      .jsonDetails(blockchain)(assetId, assetDesc, true)
                      .leftMap(err => new IllegalArgumentException(err))
                  )
              }
              .take(limit)
              .toReactivePublisher
          )
        }
      }
  }

  private def balanceJson(address: Address, assetId: IssuedAsset): JsObject =
    Json.obj(
      "address" -> address,
      "assetId" -> assetId.id.toString,
      "balance" -> JsNumber(BigDecimal(blockchain.balance(address, assetId)))
    )

  private def assetDetails(assetId: IssuedAsset, full: Boolean): Either[ApiError, JsObject] = {
    (for {
      description <- blockchain.assetDescription(assetId).toRight("Failed to get description of the asset")
      result      <- AssetsApiRoute.jsonDetails(blockchain)(assetId, description, full)
    } yield result).left.map(m => CustomValidationError(m))
  }
}

object AssetsApiRoute {
  val MAX_DISTRIBUTION_TASKS = 5

  type DistributionParams = (Int, Int, Option[Address])

  def validateDistributionParams(
      blockchain: Blockchain,
      heightParam: Int,
      limitParam: Int,
      maxLimit: Int,
      afterParam: Option[String]
  ): Either[ValidationError, DistributionParams] = {
    for {
      limit  <- validateLimit(limitParam, maxLimit)
      height <- validateHeight(blockchain, heightParam)
      after <- afterParam
        .fold[Either[ValidationError, Option[Address]]](Right(None))(addrString => Address.fromString(addrString).map(Some(_)))
    } yield (height, limit, after)
  }

  def validateHeight(blockchain: Blockchain, height: Int): Either[ValidationError, Int] = {
    for {
      _ <- Either
        .cond(height > 0, (), GenericError(s"Height should be greater than zero"))
      _ <- Either
        .cond(height != blockchain.height, (), GenericError(s"Using 'assetDistributionAtHeight' on current height can lead to inconsistent result"))
      _ <- Either
        .cond(height < blockchain.height, (), GenericError(s"Asset distribution available only at height not greater than ${blockchain.height - 1}"))
    } yield height

  }

  def validateLimit(limit: Int, maxLimit: Int): Either[ValidationError, Int] = {
    for {
      _ <- Either
        .cond(limit > 0, (), GenericError("Limit should be greater than 0"))
      _ <- Either
        .cond(limit < maxLimit, (), GenericError(s"Limit should be less than $maxLimit"))
    } yield limit
  }

  def jsonDetails(blockchain: Blockchain)(id: ByteStr, description: AssetDescription, full: Boolean): Either[String, JsObject] = {
    // (timestamp, height)
    def additionalInfo(id: ByteStr): Either[String, (Long, Int)] =
      for {
        tt <- blockchain.transactionInfo(id).toRight("Failed to find issue/invokeScript transaction by ID")
        (h, mtx) = tt
        ts <- (mtx match {
          case tx: IssueTransaction        => Some(tx.timestamp)
          case tx: InvokeScriptTransaction => Some(tx.timestamp)
          case _                           => None
        }).toRight("No issue/invokeScript transaction found with the given asset ID")
      } yield (ts, h)

    for {
      tsh <- additionalInfo(description.source)
      (timestamp, height) = tsh
      script              = description.script.filter(_ => full)
      name                = description.name.fold(bs => Base64.encode(bs.arr), identity)
      desc                = description.description.fold(bs => Base64.encode(bs.arr), identity)
    } yield JsObject(
      Seq(
        "assetId"        -> JsString(id.toString),
        "issueHeight"    -> JsNumber(height),
        "issueTimestamp" -> JsNumber(timestamp),
        "issuer"         -> JsString(description.issuer.stringRepr),
        "name"           -> JsString(name),
        "description"    -> JsString(desc),
        "decimals"       -> JsNumber(description.decimals),
        "reissuable"     -> JsBoolean(description.reissuable),
        "quantity"       -> JsNumber(BigDecimal(description.totalVolume)),
        "scripted"       -> JsBoolean(description.script.nonEmpty),
        "minSponsoredAssetFee" -> (description.sponsorship match {
          case 0           => JsNull
          case sponsorship => JsNumber(sponsorship)
        }),
        "originTransactionId" -> JsString(description.source.toString)
      ) ++ script.toSeq.map {
        case (script, complexity) =>
          "scriptDetails" -> Json.obj(
            "scriptComplexity" -> JsNumber(BigDecimal(complexity)),
            "script"           -> JsString(script.bytes().base64),
            "scriptText"       -> JsString(script.expr.toString) // [WAIT] JsString(Script.decompile(script))
          )
      }
    )
  }
}
