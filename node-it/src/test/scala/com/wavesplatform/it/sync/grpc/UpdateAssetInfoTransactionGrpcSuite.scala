package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.ApiError.{InvalidName, StateCheckFailed, TooBigArrayAllocation}
import com.wavesplatform.it.NodeConfigs.Miners
import com.wavesplatform.it.api.SyncHttpApi.assertApiError
import com.wavesplatform.it.api.{Transaction, TransactionInfo}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.sync.transactions.UpdateAssetInfoTransactionSuite
import com.wavesplatform.it.sync.transactions.UpdateAssetInfoTransactionSuite.configWithUpdateIntervalSetting
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import io.grpc.Status.Code
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.Random
import scala.concurrent.duration._

class UpdateAssetInfoTransactionGrpcSuite extends GrpcBaseTransactionSuite with TableDrivenPropertyChecks{
  import UpdateAssetInfoTransactionGrpcSuite._
  val updateInterval = 2
  override protected def nodeConfigs: Seq[Config] = Seq(configWithUpdateIntervalSetting(updateInterval).withFallback(Miners.head))

  val issuer = KeyPair("issuer".getBytes)
  val nonIssuer = KeyPair("nonIssuer".getBytes)
  var assetId   = ""
  var issueHeight = 0

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    sender.grpc.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(ByteString.copyFrom(issuer.publicKey)), transferAmount, minFee, waitForTx = true)
    sender.grpc.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(ByteString.copyFrom(nonIssuer.publicKey)), transferAmount, minFee, waitForTx = true)
    val (defaultName, defaultDescription) = ("asset", "description")
    assetId = Base58.encode(PBTransactions.vanilla(
      sender.grpc.broadcastIssue(issuer, defaultName, someAssetAmount, 8, reissuable = true, script = None, fee = issueFee, description = defaultDescription, version = 1, waitForTx = true)
    ).explicitGet().id())
    issueHeight = sender.grpc.height
  }

  test("able to update name/description of issued asset") {
    val nextTerm = issueHeight + updateInterval + 1
    sender.grpc.waitForHeight(nextTerm)
    val updateAssetInfoTxId = Base58.encode(PBTransactions.vanilla(
      sender.grpc.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)
    ).explicitGet().id())
    sender.grpc.waitForTransaction(updateAssetInfoTxId)

    sender.grpc.getTransaction(assetId).getTransaction.getUpdateAssetInfo.name shouldBe "updatedName"
    sender.grpc.getTransaction(assetId).getTransaction.getUpdateAssetInfo.description shouldBe "updatedDescription"
    sender.grpc.assetInfo(assetId).name shouldBe "updatedName"
    sender.grpc.assetInfo(assetId).description shouldBe "updatedDescription"
  }

  test("not able to update name/description more than once within interval") {
    assertGrpcError(
      sender.grpc.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee),
      "Can't update asset info before",
      Code.INVALID_ARGUMENT
    )
  }

  val invalidAssetsNames =
    Table(
      "abc",
      "UpperCaseAssetCoinTest",
      "~!|#$%^&*()_+=\";:/?><|\\][{}"
    )

  forAll(invalidAssetsNames) { assetName: String =>
    test(s"not able to update name to $assetName") {
      sender.grpc.waitForHeight(sender.height + 3)
      assertGrpcError(
        sender.grpc.updateAssetInfo(issuer, assetId, assetName, "updatedDescription", minFee),
        "invalid name",
        Code.INVALID_ARGUMENT
      )
    }
  }

  test(s"not able to set too big description") {
    val tooBigDescription = Random.nextString(1001)
    assertGrpcError(
      sender.grpc.updateAssetInfo(issuer, assetId, "updatedName", tooBigDescription, minFee),
      "Too big sequences requested",
        Code.INVALID_ARGUMENT
    )
  }

  test("not able to update asset info without paying enough fee") {
    assertGrpcError(
      sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee - 1),
      s"Fee for UpdateAssetInfoTransaction (${minFee - 1} in WAVES) does not exceed minimal value of $minFee WAVES.",
      Code.INVALID_ARGUMENT
    )
  }

  test("not able to update info of not-issued asset") {
    val notIssuedAssetId = "BzARFPgBqWFu6MHGxwkPVKmaYAzyShu495Ehsgru72Wz"
    assertGrpcError(
      sender.updateAssetInfo(issuer, notIssuedAssetId, "updatedName", "updatedDescription", minFee),
      "Referenced assetId not found",
      Code.INVALID_ARGUMENT
    )
  }

  test("non-issuer cannot update asset info") {
    assertGrpcError(
      sender.grpc.updateAssetInfo(nonIssuer, assetId, "updatedName", "updatedDescription", minFee),
      "Asset was issued by other address",
      Code.INVALID_ARGUMENT
    )
  }

}

object UpdateAssetInfoTransactionGrpcSuite {
  private def configWithUpdateIntervalSetting(interval: Long) =
    ConfigFactory.parseString(
      s"""
         |waves {
         |   blockchain.custom {
         |      functionality {
         |        min-asset-info-update-interval = $interval
         |      }
         |   }
         |   miner.quorum = 0
         |}""".stripMargin
    )
}