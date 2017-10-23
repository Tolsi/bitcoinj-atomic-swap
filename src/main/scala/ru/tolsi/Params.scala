package ru.tolsi

import org.bitcoinj.core.{Coin, NetworkParameters}

import scala.concurrent.duration.FiniteDuration

case class Params(networkParams: NetworkParameters,
                  fee: Coin,
                  timeout: FiniteDuration,
                  aliceAmount: Coin,
                  carolAmount: Coin,
                  hashX: Array[Byte],
                  startTimestamp: Long,
                  network: Network)