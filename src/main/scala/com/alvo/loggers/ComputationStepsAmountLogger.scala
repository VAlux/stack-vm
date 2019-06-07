package com.alvo.loggers

import com.alvo.VirtualMachine
import com.alvo.code.Terms.TermList


class ComputationStepsAmountLogger extends Logger[Int] {
  override def empty: Int = 0

  override def combine(x: Int, y: Int): Int = x + y

  override def logOperation: TermList => VirtualMachine[Int] => Int = _ => _ => 1
}
