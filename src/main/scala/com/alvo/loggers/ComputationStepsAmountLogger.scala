package com.alvo.loggers

import com.alvo.VirtualMachine


class ComputationStepsAmountLogger extends Logger[Int] {
  override def empty: Int = 0

  override def combine(x: Int, y: Int): Int = x + y

  override def logOperation: VirtualMachine[Int] => Int = _ => 1
}
