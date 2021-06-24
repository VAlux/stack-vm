package com.alvo.loggers

import cats.kernel.Monoid
import com.alvo.VirtualMachine.{Memory, Stack}
import com.alvo.code.Terms.Term
import com.alvo.{Action, Program, TermAction, VirtualMachine}

trait Logger[A] extends Monoid[A]:
  def logOperation: List[Term] => VirtualMachine[A] => A

object Logger:
  type StackStateLog = List[Stack]
  type MemoryStateLog = List[Memory]
  type CodeLog = List[Term]
  type StepsAmountLog = Int

  given [A: Logger]: (List[Term] => Program[A]) = terms => new Program[A] {
    override def getProgram: TermAction[A] = TermAction(
      new Action[VirtualMachine[A]] {
        override def run: VirtualMachine[A] => VirtualMachine[A] =
          vm => vm.addRecord(implicitly[Logger[A]].logOperation.apply(terms)(vm))
      }
    )
  }

  given MemoryStateLogger = new MemoryStateLogger
  given ComputationStepsAmountLogger = new ComputationStepsAmountLogger
  given StackStateLogger = new StackStateLogger
  given CodeLogger = new CodeLogger