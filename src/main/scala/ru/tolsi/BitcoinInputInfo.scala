package ru.tolsi

import org.bitcoinj.core.Coin
import org.bitcoinj.script.Script

case class BitcoinInputInfo(txId: String, outputIndex: Int, amount: Coin, script: Script, private[tolsi] val pk: Array[Byte])
