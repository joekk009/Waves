package scorex.crypto

import scorex.crypto.hash.CryptographicHash._
import scorex.crypto.hash._


object CryptographicHashImpl extends CryptographicHash {

  override val DigestSize: Int = 32

  override def hash(in: Message): Digest = applyHashes(in, Blake256, Groestl256, Keccak256, JH256, Skein256, Sha256)

}
