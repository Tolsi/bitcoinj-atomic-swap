package ru.tolsi.coinswap

import com.typesafe.scalalogging.StrictLogging
import org.bitcoinj.core.{ECKey, Transaction}
import org.bitcoinj.script.ScriptBuilder
import org.spongycastle.util.encoders.Hex
import ru.tolsi.util.TransactionsUtil._
import ru.tolsi.{BitcoinInputInfo, Params}

import scala.collection.JavaConverters._

case class BobAfter3Step(override val publicKey: Array[Byte],
                         carolPublicState: PublicState,
                         TX1id: String,
                         TX3: Transaction,
                        ) extends CarolPublicState

case class MessageBobToCarolAfter3Step(bobTx3Signature: BitcoinInputInfo)

object Bob extends StrictLogging {
  def step3(bobPrivateKey: Array[Byte],
            X: Array[Byte],
            carolPublicState: PublicState,
            m: MessageCarolToBobAfter2Step)(implicit p: Params): Either[Exception, (BobAfter3Step, MessageBobToCarolAfter3Step)] = {
    val bobPublicKey = ECKey.fromPrivate(bobPrivateKey).getPubKey

    val lockTimeCarolTs = p.startTimestamp + p.timeout.toSeconds
    val T3script = createXHashUntilTimelockOrToSelfScript(p.hashX, bobPublicKey, lockTimeCarolTs, carolPublicState.publicKey)

    val TX3Amount = p.carolAmount.minus(p.fee.multiply(2))
    val multisigScript2of2BC2 =
      ScriptBuilder.createMultiSigOutputScript(2, List(ECKey.fromPublicOnly(bobPublicKey), ECKey.fromPublicOnly(carolPublicState.publicKey)).asJava)
    val bobTx3Signature = BitcoinInputInfo(m.TX1Id, 0, multisigScript2of2BC2, bobPrivateKey)

    val TX3 = sendMoneyFromMultisig(Seq(bobTx3Signature, m.carolTX3signature), TX3Amount, T3script, _.setLockTime(lockTimeCarolTs + 1))
    logger.debug(s"Backout TX3 for Bob [${TX3.getHashAsString}] = ${Hex.toHexString(TX3.unsafeBitcoinSerialize)}")

    val TX7Amount = p.carolAmount.minus(p.fee.multiply(3))
    val TX7Bob = createBackoutTransactionByX(BitcoinInputInfo(TX3.getHashAsString, 0, T3script, bobPrivateKey), TX7Amount, X,
      ScriptBuilder.createOutputScript(ECKey.fromPublicOnly(bobPublicKey).toAddress(p.networkParams)))
    logger.debug(s"TX7 Backout from TX3 to Bob by X [${TX7Bob.getHashAsString}] = ${Hex.toHexString(TX7Bob.unsafeBitcoinSerialize)}")

    Right((BobAfter3Step(bobPublicKey, carolPublicState, m.TX1Id, TX3), MessageBobToCarolAfter3Step(bobTx3Signature)))
  }

  // step 5.5 - Bob send X to Carol after TX1 in blockchain
  def step7(bobPrivateKey: Array[Byte],
            prevState: BobAfter3Step,
            m: MessageCarolToBobAfter6Step
           )(implicit p: Params): Either[Exception, Unit] = {
    val multisigScript2of2BC2 = ScriptBuilder.createMultiSigOutputScript(2, List(ECKey.fromPublicOnly(prevState.publicKey), ECKey.fromPublicOnly(prevState.carolPublicState.publicKey)).asJava)
    val TX5Amount = p.carolAmount.minus(p.fee.multiply(2))
    val bobTx5Signature = BitcoinInputInfo(prevState.TX1id, 0, multisigScript2of2BC2, bobPrivateKey)
    val TX5 = sendMoneyFromMultisig(Seq(bobTx5Signature, m.carolTx5Signature), TX5Amount, ScriptBuilder.createOutputScript(ECKey.fromPublicOnly(prevState.publicKey).toAddress(p.networkParams)))
    p.network.sendTx(TX5, "TX5")
    Right()
  }
}
