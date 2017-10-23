package ru.tolsi

import org.bitcoinj.script.Script

case class BitcoinInputInfo(txId: String, outputIndex: Int, script: Script, private[tolsi] val pk: Array[Byte])