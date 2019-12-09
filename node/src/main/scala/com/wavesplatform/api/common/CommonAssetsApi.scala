package com.wavesplatform.api.common

import com.google.common.primitives.Shorts
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonAssetsApi.AssetInfo
import com.wavesplatform.crypto
import com.wavesplatform.database.Keys
import com.wavesplatform.state.{AssetDescription, Blockchain, Diff, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import monix.reactive.Observable
import org.iq80.leveldb.DB

trait CommonAssetsApi {
  def description(assetId: IssuedAsset): Option[AssetDescription]

  def fullInfo(assetId: IssuedAsset): Option[AssetInfo]

  def wavesDistribution(height: Int, after: Option[Address]): Observable[(Address, Long)]

  def assetDistribution(asset: IssuedAsset, height: Int, after: Option[Address]): Observable[(Address, Long)]
}

object CommonAssetsApi {
  final case class AssetInfo(description: AssetDescription, issueTransaction: IssueTransaction, sponsorBalance: Option[Long])

  def apply(diff: => Diff, db: DB, blockchain: Blockchain): CommonAssetsApi = new CommonAssetsApi {
    def description(assetId: IssuedAsset): Option[AssetDescription] = {
      blockchain.assetDescription(assetId)
    }

    def fullInfo(assetId: IssuedAsset): Option[AssetInfo] =
      for {
        assetInfo                               <- blockchain.assetDescription(assetId)
        (_, issueTransaction: IssueTransaction) <- blockchain.transactionInfo(assetId.id)
        sponsorBalance = if (assetInfo.sponsorship != 0) Some(blockchain.wavesPortfolio(issueTransaction.sender).spendableBalance) else None
      } yield AssetInfo(assetInfo, issueTransaction, sponsorBalance)

    override def wavesDistribution(height: Int, after: Option[Address]): Observable[(Address, Long)] =
      balanceDistribution(
        db,
        height,
        after,
        if (height == blockchain.height) diff.portfolios else Map.empty[Address, Portfolio],
        Shorts.toByteArray(Keys.WavesBalancePrefix),
        bs => BigInt(bs.slice(2, bs.length - 4)),
        _.balance
      )

    override def assetDistribution(asset: IssuedAsset, height: Int, after: Option[Address]): Observable[(Address, Long)] =
      balanceDistribution(
        db,
        height,
        after,
        if (height == blockchain.height) diff.portfolios else Map.empty[Address, Portfolio],
        Shorts.toByteArray(Keys.AssetBalancePrefix) ++ asset.id.arr,
        bs => BigInt(bs.slice(2 + crypto.DigestLength, bs.length - 4)),
        _.assets.getOrElse(asset, 0L)
      )
  }
}
