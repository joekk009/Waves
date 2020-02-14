package com.wavesplatform.api

import com.wavesplatform.block.serialization.BlockHeaderSerializer
import com.wavesplatform.block.{Block, BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.Json

case class BlockMeta(
    header: BlockHeader,
    signature: ByteStr,
    height: Int,
    size: Int,
    transactionCount: Int,
    totalFeeInWaves: Long,
    reward: Option[Long],
    vrf: Option[ByteStr]
) {
  def toSignedHeader: SignedBlockHeader = SignedBlockHeader(header, signature)

  val json = Coeval.evalOnce {
    BlockHeaderSerializer.toJson(header, size, transactionCount, signature) ++
      Json.obj("height" -> height, "totalFee" -> totalFeeInWaves) ++
      reward.fold(Json.obj())(r => Json.obj("reward" -> r)) ++
      vrf.fold(Json.obj())(v => Json.obj("VRF" -> v.toString))
  }
}

object BlockMeta {
  def fromBlock(block: Block, height: Int, reward: Option[Long], vrf: Option[ByteStr]): BlockMeta = BlockMeta(
    block.header,
    block.signature,
    height,
    block.bytes().length,
    block.transactionData.length,
    block.feesPortfolio().balance,
    reward,
    vrf
  )
}