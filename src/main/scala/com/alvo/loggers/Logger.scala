package com.alvo.loggers

import cats.kernel.Monoid
import com.alvo.VirtualMachine.{Memory, Stack}
import com.alvo.code.Terms.TermList
import com.alvo.{Action, Program, TermAction, VirtualMachine}

trait Logger[A] extends Monoid[A] {
  def logOperation: TermList => VirtualMachine[A] => A
}

object Logger {
  type CompositeLogger[A, B] = VirtualMachine[(A, B)] => (A, B)
  type StackStateLog = List[Stack]
  type MemoryStateLog = List[Memory]
  type CodeLog = TermList
  type StepsAmountLog = Int

  implicit def loggerProgramProvider[A: Logger]: TermList => Program[A] = terms => new Program[A] {
    override def getProgram: TermAction[A] = TermAction(new Action[VirtualMachine[A]] {
      override def run: VirtualMachine[A] => VirtualMachine[A] =
        vm => vm.addRecord(implicitly[Logger[A]].logOperation.apply(terms)(vm))
    })
  }

  implicit val memoryStateLogger: MemoryStateLogger = new MemoryStateLogger
  implicit val computationStepsAmountLogger: ComputationStepsAmountLogger = new ComputationStepsAmountLogger
  implicit val stackStateLogger: StackStateLogger = new StackStateLogger
  implicit val codeLogger: CodeLogger = new CodeLogger
}