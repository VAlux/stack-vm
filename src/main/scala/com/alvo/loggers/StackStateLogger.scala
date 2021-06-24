package com.alvo.loggers

import com.alvo.VirtualMachine
import com.alvo.code.Terms.Term
import com.alvo.loggers.Logger.StackStateLog

class StackStateLogger extends Logger[StackStateLog] {
  override def empty: StackStateLog = List.empty

  override def combine(x: StackStateLog, y: StackStateLog): StackStateLog = x ::: y

  override def logOperation: List[Term] => VirtualMachine[StackStateLog] => StackStateLog = _ => vm => List(vm.stack)
}