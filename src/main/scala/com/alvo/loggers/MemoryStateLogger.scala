package com.alvo.loggers

import com.alvo.VirtualMachine
import com.alvo.code.Terms.Term
import com.alvo.loggers.Logger.MemoryStateLog

class MemoryStateLogger extends Logger[MemoryStateLog] {
  override def empty: MemoryStateLog = List.empty

  override def combine(x: MemoryStateLog, y: MemoryStateLog): MemoryStateLog = x ::: y

  override def logOperation: List[Term] => VirtualMachine[MemoryStateLog] => MemoryStateLog = _ => vm => List(vm.memory)
}
