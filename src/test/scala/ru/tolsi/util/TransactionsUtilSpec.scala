package ru.tolsi.util

import org.bitcoinj.core.Utils
import org.scalatest.{FunSuite, Matchers}
import org.spongycastle.util.encoders.Hex

class TransactionsUtilSpec extends FunSuite with Matchers {
  test("createXHashUntilTimelockOrToSelfScript should work correct") {
    val x = Array(1, 2, 3, 4, 5).map(_.toByte)
    val oppositePublicKey = Array(1, 1, 1, 1, 1).map(_.toByte)
    val myPublicKey = Array(2, 2, 2, 2, 2).map(_.toByte)
    val timeout = 12345L

    val script = TransactionsUtil.createXHashUntilTimelockOrToSelfScript(x, oppositePublicKey, timeout, myPublicKey)
    val hashString = Hex.toHexString(Utils.sha256hash160(script.getProgram))
    hashString shouldBe "3e7df4a182d8080d024c7b34cba36046b038b175"
  }
}
