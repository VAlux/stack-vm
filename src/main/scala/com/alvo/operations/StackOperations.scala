package com.alvo.operations

import cats.kernel.{Monoid, Semigroup}
import com.alvo.Program.idF
import com.alvo.VirtualMachine.{Processor, ProgramF, Stack, error, memoryUpdate}
import com.alvo.{Program, VirtualMachine}

import scala.annotation.tailrec

object StackOperations {

  def pop[A: Monoid]: ProgramF[A] = Program.createProgramForStack { stack =>
    vm =>
      stack match {
        case _ :: xs => vm.setStack(xs)
        case _ => error("pop action expected an argument").apply(vm)
      }
  }

  def push[A: Monoid](element: Int): ProgramF[A] = Program.createProgramForStack { stack =>
    vm =>
      vm.setStack(element :: stack)
  }

  def dup[A: Monoid]: ProgramF[A] = Program.createProgramForStack { stack =>
    vm =>
      stack match {
        case x :: xs => vm.setStack(x :: x :: xs)
        case _ => error("dup action expects an argument").apply(vm)
      }
  }

  def swap[A: Monoid]: ProgramF[A] = Program.createProgramForStack { stack =>
    vm =>
      stack match {
        case x :: y :: xs => vm.setStack(y :: x :: xs)
        case _ => error("swap action expects 2 arguments").apply(vm)
      }
  }

  def exch[A: Monoid]: ProgramF[A] = Program.createProgramForStack { stack =>
    vm =>
      stack match {
        case x :: y :: xs => vm.setStack(y :: x :: y :: xs)
        case _ => error("exch action expects 2 arguments").apply(vm)
      }
  }

  def put[A: Monoid](index: Int = 0): ProgramF[A] = Program.createIndexedProgram(index).apply {
    (stack, memory) =>
      vm =>
        stack match {
          case x :: xs => vm.setStack(xs).setMemory(memoryUpdate(memory, index, x))
          case _ => error("put action expects an argument").apply(vm)
        }
  }

  def get[A: Monoid](index: Int = 0): ProgramF[A] = Program.createIndexedProgram(index).apply {
    (stack, memory) =>
      vm => vm.setStack(memory(index) :: stack)
  }

  def proceed[A: Monoid]: Program[A] => ProgramF[A] => Stack => Processor[A] =
    context => program => stack => program.apply(context).getProgram.run.compose(_.setStack(stack))

  def branch[A: Monoid](branch1: ProgramF[A])(branch2: ProgramF[A])(implicit context: Program[A]): ProgramF[A] =
    Program.createProgramForStack { stack =>
      vm =>
        stack match {
          case x :: xs => proceed.apply(context)(if (x != 0) branch1 else branch2)(xs)(vm)
          case _ => error("branch requires an argument").apply(vm)
        }
    }

  def rep[A: Monoid](body: ProgramF[A])(implicit context: Program[A]): ProgramF[A] =
    Program.createProgramForStack { stack =>
      vm =>
        stack match {
          case x :: xs => proceed.apply(context)(Semigroup[ProgramF[A]].combineN(body, x))(xs)(vm)
          case _ => error("rep operation required an argument").apply(vm)
        }
    }

  def loop[A: Monoid](test: ProgramF[A])(body: ProgramF[A])(implicit context: Program[A]): ProgramF[A] =
    Program.createProgramForStack { _ =>
      vm => {
        @tailrec
        def iterate(machine: VirtualMachine[A]): VirtualMachine[A] = {
          proceed.apply(context)(test)(machine.stack)(machine).stack match {
            case 0 :: xs => proceed.apply(context)(idF)(xs)(machine)
            case _ :: xs => iterate(proceed.apply(context)(body)(xs)(machine))
            case _ => error("loop operation required an argument").apply(machine)
          }
        }

        iterate(vm)
      }
    }
}
