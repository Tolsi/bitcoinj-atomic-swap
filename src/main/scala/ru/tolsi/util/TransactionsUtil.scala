package ru.tolsi.util

import org.bitcoinj.core._
import org.bitcoinj.core.Transaction.SigHash
import org.bitcoinj.script.{Script, ScriptBuilder}
import org.bitcoinj.script.ScriptOpCodes._
import ru.tolsi.{BitcoinInputInfo, Params}

import scala.collection.JavaConverters._

object TransactionsUtil {
  def createXHashUntilTimelockOrToSelfScript(digest: Array[Byte], oppositePublicKey: Array[Byte], timeout: Long, myPublicKey: Array[Byte]): Script = {
    new ScriptBuilder().op(OP_DEPTH).op(OP_2).op(OP_EQUAL).op(OP_IF)
      .op(OP_HASH160).data(digest).op(OP_EQUALVERIFY).data(oppositePublicKey).op(OP_CHECKSIG)
      .op(OP_ELSE).number(timeout).op(OP_CHECKLOCKTIMEVERIFY).op(OP_DROP).data(myPublicKey).op(OP_CHECKSIG)
      .op(OP_ENDIF)
      .build
  }

  def sendMoneyFromMultisig(txInputs: Seq[BitcoinInputInfo], v: Coin, outputScript: Script)(implicit p: Params): Transaction = {
    require(txInputs.forall(t => t.outputIndex == txInputs.head.outputIndex && t.script == txInputs.head.script && t.txId == txInputs.head.txId))
    val tx = new Transaction(p.networkParams)
    tx.setPurpose(Transaction.Purpose.USER_PAYMENT)
    tx.addOutput(v, outputScript)
    val headInput = txInputs.head
    val input = tx.addInput(Sha256Hash.wrap(headInput.txId), headInput.outputIndex, headInput.script)
    input.setSequenceNumber(0)
    val sings = txInputs.map { txInput =>
      val pk = ECKey.fromPrivate(txInput.pk)
      tx.calculateSignature(0, pk, txInput.script, SigHash.ALL, false)
    }
    input.setScriptSig(ScriptBuilder.createMultiSigInputScript(sings.asJava))
    tx
  }

  def sendMoneyToScript(txInput: BitcoinInputInfo, v: Coin, outputScript: Script)(implicit p: Params): Transaction = {
    val tx = new Transaction(p.networkParams)
    tx.setPurpose(Transaction.Purpose.USER_PAYMENT)
    tx.addOutput(v, outputScript)

    val input = tx.addInput(Sha256Hash.wrap(txInput.txId), txInput.outputIndex, txInput.script)
    val pk = ECKey.fromPrivate(txInput.pk)
    input.setScriptSig(ScriptBuilder.createInputScript(tx.calculateSignature(0, pk, txInput.script, SigHash.ALL, false), pk))
    tx
  }

  def createBackoutTransactionByTimeout(txInput: BitcoinInputInfo, v: Coin, outputScript: Script)(implicit p: Params): Transaction = {
    val tx = new Transaction(p.networkParams)
    tx.setPurpose(Transaction.Purpose.USER_PAYMENT)
    tx.addOutput(v, outputScript)

    val input = tx.addInput(Sha256Hash.wrap(txInput.txId), txInput.outputIndex, txInput.script)
    val pk = ECKey.fromPrivate(txInput.pk)
    val signature = tx.calculateSignature(0, pk, input.getScriptSig, SigHash.ALL, false)
    input.setScriptSig(new ScriptBuilder().data(signature.encodeToBitcoin()).build())
    tx
  }

  def createBackoutTransactionByX(txInput: BitcoinInputInfo, v: Coin, x: Array[Byte], outputScript: Script)(implicit p: Params): Transaction = {
    val tx = new Transaction(p.networkParams)
    tx.setPurpose(Transaction.Purpose.USER_PAYMENT)
    tx.addOutput(v, outputScript)

    val input = tx.addInput(Sha256Hash.wrap(txInput.txId), txInput.outputIndex, txInput.script)
    val pk = ECKey.fromPrivate(txInput.pk)
    val signature = tx.calculateSignature(0, pk, input.getScriptSig, SigHash.ALL, false)
    input.setScriptSig(new ScriptBuilder().data(signature.encodeToBitcoin()).data(x).build())
    tx
  }
}
