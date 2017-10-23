package ru.tolsi

import com.typesafe.scalalogging.StrictLogging
import org.bitcoinj.core.Transaction
import org.spongycastle.util.encoders.Hex

trait Network {
  def sendTx(tx: Transaction, name: String): Unit
}

object ConsoleFakeNetwork extends Network with StrictLogging {
  override def sendTx(t: Transaction, name: String): Unit = {
    val hex = Hex.toHexString(t.unsafeBitcoinSerialize)
    val id = t.getHashAsString
    logger.info(s"$name tx id [$id]:")
    logger.info(hex)
  }
}
