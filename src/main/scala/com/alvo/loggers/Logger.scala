package com.alvo.loggers

import cats.kernel.Monoid
import com.alvo.VirtualMachine
import com.alvo.VirtualMachine.{Memory, Stack}

trait Logger[A] extends Monoid[A] {
  def logOperation: VirtualMachine[A] => A
}

object Logger {
  type CompositeLogger[A, B] = VirtualMachine[(A, B)] => (A, B)
  type StackStateLog = List[Stack]
  type MemoryStateLog = List[Memory]
  type StepsAmountLog = Int

  implicit def loggerOperationProvider[A: Logger]: VirtualMachine[A] => A = implicitly[Logger[A]].logOperation
}