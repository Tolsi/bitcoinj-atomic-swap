package ru.tolsi.coinswap

import com.typesafe.scalalogging.StrictLogging
import org.bitcoinj.core.{ECKey, Transaction}
import org.bitcoinj.script.ScriptBuilder
import org.spongycastle.util.encoders.Hex
import ru.tolsi.util.TransactionsUtil._
import ru.tolsi.{BitcoinInputInfo, Params}

import scala.collection.JavaConverters._

trait PublicState {
  def publicKey: Array[Byte]
}

trait AlicePublicState extends PublicState

trait CarolPublicState extends PublicState

trait BobPublicState extends PublicState

case class AliceAfter1Step(override val publicKey: Array[Byte],
                           carolPublicState: CarolPublicState,
                           TX0: Transaction,
                           aliceTX2signature: BitcoinInputInfo) extends AlicePublicState

case class AliceAfter4Step(override val publicKey: Array[Byte],
                           prevState: AliceAfter1Step,
                           TX2: Transaction) extends AlicePublicState

case class MessageAliceToBobAfter1Step(
                                        TX0Id: String,
                                        aliceTX2signature: BitcoinInputInfo)

case class MessageAliceToCarolAfter8Step(
                                          aliceTx4Signature: BitcoinInputInfo)

object Alice extends StrictLogging {
  def step1(alicePrivateKey: Array[Byte],
            aliceInput: BitcoinInputInfo,
            carolPublicState: CarolPublicState)(implicit p: Params): Either[Exception, (AliceAfter1Step, MessageAliceToBobAfter1Step)] = {
    val alicePublicKey = ECKey.fromPrivate(alicePrivateKey).getPubKey

    // 2-2-A1-C0
    val multisigScript2of2BC1 = ScriptBuilder.createMultiSigOutputScript(2, List(ECKey.fromPublicOnly(alicePublicKey), ECKey.fromPublicOnly(carolPublicState.publicKey)).asJava)

    val TX0Amount = p.aliceAmount.minus(p.fee)
    val TX0 = sendMoneyToScript(aliceInput, TX0Amount, multisigScript2of2BC1)
    val TX0id = TX0.getHashAsString

    val aliceTX2signature = BitcoinInputInfo(TX0id, 0, aliceInput.amount.minus(p.fee.multiply(2)), multisigScript2of2BC1, alicePrivateKey)

    Right(AliceAfter1Step(alicePublicKey, carolPublicState, TX0, aliceTX2signature), MessageAliceToBobAfter1Step(TX0id, aliceTX2signature))
  }

  def step4(alicePrivateKey: Array[Byte],
            m: MessageCarolToAliceAfter2Step,
            prevState: AliceAfter1Step)(implicit p: Params): Either[Exception, AliceAfter4Step] = {
    // todo verify m.carolTX2signature
    p.network.sendTx(prevState.TX0, "TX0")
    val multisigScript2of2BC1 = ScriptBuilder.createMultiSigOutputScript(2, List(ECKey.fromPublicOnly(prevState.publicKey), ECKey.fromPublicOnly(prevState.carolPublicState.publicKey)).asJava)
    val TX2Amount = p.aliceAmount.minus(p.fee.multiply(2))
    val aliceTX2signature = BitcoinInputInfo(prevState.TX0.getHashAsString, 0, TX2Amount, multisigScript2of2BC1, alicePrivateKey)
    val lockTimeAliceTs = p.startTimestamp + p.timeout.toSeconds * 2
    val T2script = createXHashUntilTimelockOrToSelfScript(p.hashX, prevState.carolPublicState.publicKey, lockTimeAliceTs, prevState.publicKey)
    // todo may be use P2SH for working OP_CHECKLOCKTIMEVERIFY ?
    val TX2 = sendMoneyFromMultisig(Seq(aliceTX2signature, m.carolTX2signature), TX2Amount, T2script)
    logger.debug(s"Backout TX2 for Alice [${TX2.getHashAsString}] = ${Hex.toHexString(TX2.unsafeBitcoinSerialize)}")
    val TX6Amount = p.aliceAmount.minus(p.fee.multiply(3))
    // todo it works in any case :<
    val TX6Alice = createBackoutTransactionByTimeout(BitcoinInputInfo(TX2.getHashAsString, 0, p.aliceAmount.minus(p.fee.multiply(2)), T2script, alicePrivateKey), TX6Amount,
      ScriptBuilder.createOutputScript(ECKey.fromPublicOnly(prevState.publicKey).toAddress(p.networkParams)))
    logger.debug(s"TX6 Backout from TX2 for Alice by timeout [${TX6Alice.getHashAsString}] = ${Hex.toHexString(TX6Alice.unsafeBitcoinSerialize)}")

    Right(AliceAfter4Step(prevState.publicKey, prevState, TX2))
  }

  def step8(alicePrivateKey: Array[Byte],
            prevState: AliceAfter4Step
           )(implicit p: Params): Either[Exception, MessageAliceToCarolAfter8Step] = {
    val multisigScript2of2BC1 = ScriptBuilder.createMultiSigOutputScript(2, List(ECKey.fromPublicOnly(prevState.publicKey), ECKey.fromPublicOnly(prevState.prevState.carolPublicState.publicKey)).asJava)
    val aliceTx4Signature = BitcoinInputInfo(prevState.prevState.TX0.getHashAsString, 0, p.aliceAmount.minus(p.fee.multiply(2)), multisigScript2of2BC1, alicePrivateKey)
    Right(MessageAliceToCarolAfter8Step(aliceTx4Signature))
  }
}
