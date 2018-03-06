package ru.tolsi

import com.typesafe.scalalogging.StrictLogging
import org.bitcoinj.core.{Base58, Coin, ECKey, Utils}
import org.bitcoinj.params.TestNet3Params
import org.bitcoinj.script.ScriptBuilder
import ru.tolsi.coinswap._

import scala.concurrent.duration._

object CoinSwap extends App with StrictLogging {
  def buildTestParams(
                       hashX: Array[Byte],
                       aliceAmount: Coin,
                       carolAmount: Coin,
                       now: Long
                     ): Params = Params(
    TestNet3Params.get(),
    // 0.01 BTC
    fee = Coin.CENT,
    timeout = 1 minute,
    aliceAmount,
    carolAmount,
    hashX,
    now,
    ConsoleFakeNetwork)

  def privateKeyBytesFromWIF(wif: String): Array[Byte] = {
     val wifBytes = Base58.decode(wif)
    val withoutMetaData = wifBytes.dropRight(4).drop(1)
    // is public key is compressed
    if (withoutMetaData.last == 1) {
      withoutMetaData.dropRight(1)
    } else withoutMetaData
  }

  private val alicePk = privateKeyBytesFromWIF("cMceqPfyJdTT2tDycQBWBUGcjeoedF2v8dZBzc1svRj9jFAiLXVA")
  private val alicePublic = new AlicePublicState {
    override def publicKey: Array[Byte] = ECKey.fromPrivate(alicePk).getPubKey
  }

  private val carolPk = privateKeyBytesFromWIF("cMceqPhHMLdvTKwTxeTWntcaP4z6FpdCr3MqH8LheqipaqmeaYUU")
  private val carolPublic = new CarolPublicState {
    override def publicKey: Array[Byte] = ECKey.fromPrivate(carolPk).getPubKey
  }

  private val bobX = "I don't like alice".getBytes

  implicit private val p = buildTestParams(
    Utils.sha256hash160(bobX),
    Coin.parseCoin("1.83211665"),
    Coin.parseCoin("0.25749999"),
    1508846461
  )

  val aliceEC = ECKey.fromPrivate(alicePk)
  val carolEC = ECKey.fromPrivate(carolPk)
  logger.info(s"Alice ${ECKey.fromPrivate(alicePk).toAddress(p.networkParams)} [public key: ${aliceEC.getPublicKeyAsHex}]")
  logger.info(s"Carol ${carolEC.toAddress(p.networkParams)} [public key: ${carolEC.getPublicKeyAsHex}]")
  logger.info(s"Bob ${ECKey.fromPrivate(alicePk).toAddress(p.networkParams)} [public key: ${aliceEC.getPublicKeyAsHex}]")

  private val aliceOutInfoBC1 = BitcoinInputInfo("d4873e0c88457b1c31322bd8d3fce738e7ad426bdbe44c588d67e9b571b19e04", 0,
    ScriptBuilder.createOutputScript(ECKey.fromPrivate(alicePk).toAddress(p.networkParams)), alicePk)

  private val carolOutInfoBC2 = BitcoinInputInfo("5f15ac91050d2de5a3d6e1db5a1787a73c209dfb6b2b3919573705289cdccdb9", 0,
    ScriptBuilder.createOutputScript(ECKey.fromPrivate(carolPk).toAddress(p.networkParams)), carolPk)

  // TX0 - Alice -> Multisig1[Alice, Carol]
  // TX1 - Carol -> Multisig2[Bob, Carol]
  // TX2 - Alice + Carol [uncooperative case] - Multisig1[Alice, Carol] -> scr1
  // TX3 - Bob + Carol [uncooperative case] - Multisig2[Bob, Carol] -> scr2
  // TX4 - Alice + Carol -> [cooperative case] - Multisig1[Alice, Carol] -> Carol
  // TX5 - Bob + Carol -> [cooperative case] - Multisig2[Bob, Carol] -> Alice
  // TX6 - Alice or Carol [failed case] - scr1 (TX2) -> Alice or Carol
  // TX7 - Bob or Carol [failed case] - scr2 (TX3) -> Multisig2[Bob, Carol]

  val (alice1, atc1) = Alice.step1(alicePk, aliceOutInfoBC1, carolPublic).right.get
  val (carol2, cta1, ctb1) = Carol.step2(carolPk, carolOutInfoBC2, alicePublic, alicePublic, atc1).right.get
  val (bob3, btc1) = Bob.step3(alicePk, bobX, carolPublic, ctb1).right.get
  // broadcast tx0 and tx1
  val alice4 = Alice.step4(alicePk, cta1, alice1).right.get
  val carol5 = Carol.step5(carolPk, carol2, btc1).right.get
  // wait for tx0 and tx1, X
  val (carol6, ctb2) = Carol.step6(carolPk, carol5, bobX).right.get
  Bob.step7(alicePk, bob3, ctb2).right.get
  val atc2 = Alice.step8(alicePk, alice4).right.get
  Carol.step9(carolPk, carol6, atc2).right.get
  logger.info("Done")
}
