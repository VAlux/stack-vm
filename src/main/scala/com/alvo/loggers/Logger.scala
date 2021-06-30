package com.alvo.loggers

import cats.kernel.Monoid
import com.alvo.VirtualMachine.{Memory, Stack}
import com.alvo.code.Term
import com.alvo.{Action, Program, TermAction, VirtualMachine}

trait Logger[A] extends Monoid[A]:
  def logOperation: List[Term] => VirtualMachine[A] => A

object Logger:
  type StackStateLog = List[Stack]
  type MemoryStateLog = List[Memory]
  type CodeLog = List[Term]
  type StepsAmountLog = Int

  given [A: Logger]: (List[Term] => Program[A]) = terms =>
    new Program[A]:
      override def getProgram: TermAction[A] =
        TermAction(
          terms,
          new Action[VirtualMachine[A]]:
            override def run: VirtualMachine[A] => VirtualMachine[A] =
              vm => vm.addRecord(summon[Logger[A]].logOperation.apply(terms)(vm))
        )

  given memoryStateLogger: Logger[MemoryStateLog] with
    def empty: MemoryStateLog = List.empty
    def combine(x: MemoryStateLog, y: MemoryStateLog): MemoryStateLog = x ::: y
    def logOperation: List[Term] => VirtualMachine[MemoryStateLog] => MemoryStateLog = _ => vm => List(vm.memory)

  given computationStepsAmountLogger: Logger[Int] with
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
    def logOperation: List[Term] => VirtualMachine[Int] => Int = _ => _ => 1

  given stackStateLogger: Logger[StackStateLog] with
    def empty: StackStateLog = List.empty
    def combine(x: StackStateLog, y: StackStateLog): StackStateLog = x ::: y
    def logOperation: List[Term] => VirtualMachine[StackStateLog] => StackStateLog = _ => vm => List(vm.stack)

  given codeLogger: Logger[List[Term]] with
    def empty: List[Term] = List.empty
    def combine(x: List[Term], y: List[Term]): List[Term] = x ::: y
    def logOperation: List[Term] => VirtualMachine[List[Term]] => List[Term] = terms => _ => terms

  given compositeLogger[A: Logger, B: Logger]: Logger[(A, B)] with
    private[this] val logA: Logger[A] = summon[Logger[A]]
    private[this] val logB: Logger[B] = summon[Logger[B]]

    def empty: (A, B) = (logA.empty, logB.empty)

    def combine(a: (A, B), b: (A, B)): (A, B) = (a, b) match
      case ((a1, b1), (a2, b2)) => (logA.combine(a1, a2), logB.combine(b1, b2))

    def logOperation: List[Term] => VirtualMachine[(A, B)] => (A, B) =
      terms =>
        vm =>
          val vmA = vm.copy(journal = vm.journal._1)
          val vmB = vm.copy(journal = vm.journal._2)
          (logA.logOperation.apply(terms)(vmA), logB.logOperation.apply(terms)(vmB))
