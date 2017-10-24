package ru.tolsi.coinswap

import com.typesafe.scalalogging.StrictLogging
import org.bitcoinj.core.{ECKey, Transaction}
import org.bitcoinj.script.{Script, ScriptBuilder}
import org.spongycastle.util.encoders.Hex
import ru.tolsi.util.TransactionsUtil._
import ru.tolsi.{BitcoinInputInfo, Params}

import scala.collection.JavaConverters._

case class CarolAfter2Step(override val publicKey: Array[Byte],
                           alicePublicState: AlicePublicState,
                           bobPublicState: PublicState,
                           TX0id: String,
                           TX1: Transaction,
                           TX2: Transaction,
                           carolTx3Signature: BitcoinInputInfo,
                           T2script: Script,
                           T3script: Script,
                          ) extends CarolPublicState

case class CarolAfter5Step(
                            override val publicKey: Array[Byte],
                            prevState: CarolAfter2Step,
                            TX3: Transaction
                          ) extends CarolPublicState

case class CarolAfter6Step(
                            override val publicKey: Array[Byte],
                            prevState: CarolAfter5Step
                          ) extends CarolPublicState

case class MessageCarolToAliceAfter2Step(carolTX2signature: BitcoinInputInfo)

case class MessageCarolToBobAfter2Step(TX1Id: String,
                                       carolTX3signature: BitcoinInputInfo)

case class MessageCarolToBobAfter6Step(carolTx5Signature: BitcoinInputInfo)

object Carol extends StrictLogging {
  def step2(carolPrivateKey: Array[Byte],
            carolInput: BitcoinInputInfo,
            alicePublicState: AlicePublicState,
            bobPublicState: PublicState,
            m: MessageAliceToBobAfter1Step)(implicit p: Params): Either[Exception, (CarolAfter2Step, MessageCarolToAliceAfter2Step, MessageCarolToBobAfter2Step)] = {
    val carolPublicKey = ECKey.fromPrivate(carolPrivateKey).getPubKey
    // 2-2-A3-C4
    val multisigScript2of2BC1 = ScriptBuilder.createMultiSigOutputScript(2, List(ECKey.fromPublicOnly(alicePublicState.publicKey), ECKey.fromPublicOnly(carolPublicKey)).asJava)
    val multisigScript2of2BC2 = ScriptBuilder.createMultiSigOutputScript(2, List(ECKey.fromPublicOnly(bobPublicState.publicKey), ECKey.fromPublicOnly(carolPublicKey)).asJava)
    val TX1Amount = p.carolAmount.minus(p.fee)
    val TX1 = sendMoneyToScript(carolInput, TX1Amount, multisigScript2of2BC2)

    val lockTimeCarolTs = p.startTimestamp + p.timeout.toSeconds
    val lockTimeAliceTs = lockTimeCarolTs + p.timeout.toSeconds

    val T2script = createXHashUntilTimelockOrToSelfScript(p.hashX, carolPublicKey, lockTimeAliceTs, alicePublicState.publicKey)
    val T3script = createXHashUntilTimelockOrToSelfScript(p.hashX, bobPublicState.publicKey, lockTimeCarolTs, carolPublicKey)

    val TX2Amount = p.aliceAmount.minus(p.fee.multiply(2))
    val carolTx2Signature = BitcoinInputInfo(m.TX0Id, 0, multisigScript2of2BC1, carolPrivateKey)
    // todo verify m.aliceTX2signature
    val TX2 = sendMoneyFromMultisig(Seq(m.aliceTX2signature, carolTx2Signature), TX2Amount, T2script, _.setLockTime(lockTimeAliceTs + 1))
    logger.debug(s"Backout TX2 for Carol [${TX2.getHashAsString}] = ${Hex.toHexString(TX2.unsafeBitcoinSerialize)}")

    val carolTx3Signature = BitcoinInputInfo(TX1.getHashAsString, 0, multisigScript2of2BC2, carolPrivateKey)

    Right(
      CarolAfter2Step(carolPublicKey, alicePublicState, bobPublicState, m.TX0Id, TX1, TX2, carolTx3Signature, T2script, T3script),
      MessageCarolToAliceAfter2Step(carolTx2Signature),
      MessageCarolToBobAfter2Step(TX1.getHashAsString, carolTx3Signature)
    )
  }

  def step5(carolPrivateKey: Array[Byte],
            prevState: CarolAfter2Step,
            m: MessageBobToCarolAfter3Step
           )(implicit p: Params): Either[Exception, CarolAfter5Step] = {
    val TX3Amount = p.carolAmount.minus(p.fee.multiply(2))
    // todo validate m.bobTx3Signature
    val lockTimeCarolTs = p.startTimestamp + p.timeout.toSeconds
    val TX3 = sendMoneyFromMultisig(Seq(m.bobTx3Signature, prevState.carolTx3Signature), TX3Amount, prevState.T3script, _.setLockTime(lockTimeCarolTs + 1))
    logger.debug(s"Backout TX3 for Carol [${TX3.getHashAsString}] = ${Hex.toHexString(TX3.unsafeBitcoinSerialize)}")
    val TX7Amount = p.carolAmount.minus(p.fee.multiply(3))
    // todo it works in any case :<
    val TX7Carol = createBackoutTransactionByTimeout(BitcoinInputInfo(TX3.getHashAsString, 0, prevState.T3script, carolPrivateKey), TX7Amount,
      ScriptBuilder.createOutputScript(ECKey.fromPublicOnly(prevState.publicKey).toAddress(p.networkParams)))
    logger.debug(s"TX7 Backout from TX3 to Carol by timeout [${TX7Carol.getHashAsString}] = ${Hex.toHexString(TX7Carol.unsafeBitcoinSerialize)}")
    p.network.sendTx(prevState.TX1, "TX1")

    Right(CarolAfter5Step(prevState.publicKey, prevState, TX3))
  }

  // step 5.5 - Carol wait for TX0 in blockchain and X from Bob
  def step6(carolPrivateKey: Array[Byte],
            prevState: CarolAfter5Step,
            X: Array[Byte]
           )(implicit p: Params): Either[Exception, (CarolAfter6Step, MessageCarolToBobAfter6Step)] = {
    val multisigScript2of2BC2 = ScriptBuilder.createMultiSigOutputScript(2, List(ECKey.fromPublicOnly(prevState.prevState.bobPublicState.publicKey), ECKey.fromPublicOnly(prevState.publicKey)).asJava)
    val TX5Amount = p.carolAmount.minus(p.fee.multiply(2))
    val carolTx5Signature = BitcoinInputInfo(prevState.prevState.TX1.getHashAsString, 0, multisigScript2of2BC2, carolPrivateKey)

    val TX6Amount = p.aliceAmount.minus(p.fee.multiply(3))
    val TX6Carol = createBackoutTransactionByX(BitcoinInputInfo(prevState.prevState.TX2.getHashAsString, 0, prevState.prevState.T2script, carolPrivateKey), TX6Amount, X,
      ScriptBuilder.createOutputScript(ECKey.fromPublicOnly(prevState.publicKey).toAddress(p.networkParams)))
    logger.debug(s"TX6 Backout from TX2 for Carol by X [${TX6Carol.getHashAsString}] = ${Hex.toHexString(TX6Carol.unsafeBitcoinSerialize)}")

    Right((CarolAfter6Step(prevState.publicKey, prevState), MessageCarolToBobAfter6Step(carolTx5Signature)))
  }

  def step9(carolPrivateKey: Array[Byte],
            prevState: CarolAfter6Step,
            m: MessageAliceToCarolAfter8Step
           )(implicit p: Params): Either[Exception, Unit] = {
    val multisigScript2of2BC1 = ScriptBuilder.createMultiSigOutputScript(2, List(ECKey.fromPublicOnly(prevState.prevState.prevState.alicePublicState.publicKey), ECKey.fromPublicOnly(prevState.prevState.publicKey)).asJava)
    val carolTx4Signature = BitcoinInputInfo(prevState.prevState.prevState.TX0id, 0, multisigScript2of2BC1, carolPrivateKey)
    val TX4Amount = p.aliceAmount.minus(p.fee.multiply(2))
    val TX4 = sendMoneyFromMultisig(Seq(m.aliceTx4Signature, carolTx4Signature), TX4Amount,
      ScriptBuilder.createOutputScript(ECKey.fromPublicOnly(prevState.publicKey).toAddress(p.networkParams)))
    p.network.sendTx(TX4, "TX4")
    Right()
  }
}
