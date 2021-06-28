package com.alvo.operations

import cats.kernel.{Monoid, Semigroup}
import com.alvo.Program.idF
import com.alvo.VirtualMachine.{Processor, ProgramF, Stack, error, memoryUpdate}
import com.alvo.code.Term
import com.alvo.code.Term.*
import com.alvo.{Program, VirtualMachine}
import scala.language.implicitConversions
import scala.annotation.tailrec

object StackOperations:

  def pop[A: Monoid]: ProgramF[A] = Program.createProgramForStack(POP) { stack =>
    vm =>
      stack match
        case _ :: xs => vm.setStack(xs)
        case _ => error("pop action expected an argument").apply(vm)
  }

  def push[A: Monoid](element: Int): ProgramF[A] = Program.createProgramForStack(PUSH(element)) { stack =>
    vm =>
      vm.setStack(element :: stack)
  }

  def dup[A: Monoid]: ProgramF[A] = Program.createProgramForStack(DUP) { stack =>
    vm =>
      stack match
        case x :: xs => vm.setStack(x :: x :: xs)
        case _ => error("dup action expects an argument").apply(vm)
  }

  def swap[A: Monoid]: ProgramF[A] = Program.createProgramForStack(SWAP) { stack =>
    vm =>
      stack match
        case x :: y :: xs => vm.setStack(y :: x :: xs)
        case _ => error("swap action expects 2 arguments").apply(vm)
  }

  def exch[A: Monoid]: ProgramF[A] = Program.createProgramForStack(EXCH) { stack =>
    vm =>
      stack match
        case x :: y :: xs => vm.setStack(y :: x :: y :: xs)
        case _ => error("exch action expects 2 arguments").apply(vm)
  }

  def put[A: Monoid](index: Int = 0): ProgramF[A] = Program.createIndexedProgram(PUT(index))(index).apply {
    (stack, memory) =>
      vm =>
        stack match
          case x :: xs => vm.setStack(xs).setMemory(memoryUpdate(memory, index, x))
          case _ => error("put action expects an argument").apply(vm)
  }

  def get[A: Monoid](index: Int = 0): ProgramF[A] = Program.createIndexedProgram(GET(index))(index).apply {
    (stack, memory) =>
      vm => vm.setStack(memory(index) :: stack)
  }

  def proceed[A: Monoid]: Program[A] => ProgramF[A] => Stack => Processor[A] =
    context => program => stack => program.apply(context).getProgram.action.run.compose(_.setStack(stack))

  def branch[A: Monoid](termList1: List[Term], termList2: List[Term])
                       (branch1: ProgramF[A])(branch2: ProgramF[A])
                       (implicit context: List[Term] => Program[A]): ProgramF[A] =
    Program.createProgramForStack(IF(termList1, termList2)) { stack =>
      vm =>
        stack match
          case x :: xs => proceed.apply(context(IF(termList1, termList2)))(if (x != 0) branch1 else branch2)(xs)(vm)
          case _ => error("branch requires an argument").apply(vm)
    }

  def rep[A: Monoid](termList: List[Term])(body: ProgramF[A])(using context: List[Term] => Program[A]): ProgramF[A] =
    Program.createProgramForStack(REP(termList)) { stack =>
      vm =>
        stack match
          case x :: xs => proceed.apply(context(REP(termList)))(Semigroup[ProgramF[A]].combineN(body, x))(xs)(vm)
          case _ => error("rep operation required an argument").apply(vm)
    }

  def loop[A: Monoid](testTerm: List[Term], bodyTerm: List[Term])
                     (test: ProgramF[A])
                     (body: ProgramF[A])
                     (implicit context: List[Term] => Program[A]): ProgramF[A] =
    Program.createProgramForStack(WHILE(testTerm, bodyTerm)) { _ =>
      vm => {
        @tailrec
        def iterate(machine: VirtualMachine[A]): VirtualMachine[A] =
          proceed.apply(context(WHILE(testTerm, bodyTerm)))(test)(machine.stack)(machine).stack match
            case 0 :: xs => proceed.apply(context(WHILE(testTerm, bodyTerm)))(idF)(xs)(machine)
            case _ :: xs => iterate(proceed.apply(context(WHILE(testTerm, bodyTerm)))(body)(xs)(machine))
            case _ => error("loop operation required an argument").apply(machine)

        iterate(vm)
      }
    }

end StackOperations