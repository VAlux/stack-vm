package com.alvo.loggers

import com.alvo.VirtualMachine
import com.alvo.loggers.Logger.StackStateLog

class StackStateLogger extends Logger[StackStateLog] {
  override def empty: StackStateLog = List.empty

  override def combine(x: StackStateLog, y: StackStateLog): StackStateLog = x ::: y

  override def logOperation: VirtualMachine[StackStateLog] => StackStateLog = vm => List(vm.stack)
}

object StackStateLogger {
  implicit val stackStateLogger: StackStateLogger = new StackStateLogger
}