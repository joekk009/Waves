package com.wavesplatform.state

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.script.Script

case class AccountScriptInfo(publicKey: PublicKey, script: Script, verifierComplexity: Long, callableComplexity: Map[String, Long] = Map.empty)
