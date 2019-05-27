package com.alvo.loggers

import com.alvo.VirtualMachine
import com.alvo.loggers.Logger.MemoryStateLog

class MemoryStateLogger extends Logger[MemoryStateLog] {
  override def empty: MemoryStateLog = List.empty

  override def combine(x: MemoryStateLog, y: MemoryStateLog): MemoryStateLog = x ::: y

  override def logOperation: VirtualMachine[MemoryStateLog] => MemoryStateLog = vm => List(vm.memory)
}
