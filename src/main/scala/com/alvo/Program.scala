package com.alvo

import cats.kernel.Monoid
import com.alvo.VirtualMachine.{Memory, Processor, ProgramF, Stack}

trait Program[A] {
  def getProgram: Action[VirtualMachine[A]]
}

object Program {

  type VirtualMachineF[A] = VirtualMachine[A] => VirtualMachine[A]

  type ProcessorF[A] = (Stack, Memory) => Processor[A]

  def id[A: Monoid]: Program[A] = new Program[A] {
    override def getProgram: Action[VirtualMachine[A]] = new Action[VirtualMachine[A]] {
      override def run: VirtualMachineF[A] = identity
    }
  }

  def idF[A: Monoid]: ProgramF[A] = _ => Program.id

  implicit def programId[A: Monoid]: Program[A] = Program.id

  def createProgramForStack[A: Monoid](program: Stack => Processor[A]): ProgramF[A] = context => new Program[A] {
    override def getProgram: Action[VirtualMachine[A]] = new Action[VirtualMachine[A]] {
      override val run: VirtualMachineF[A] = vm => vm.status match {
        case Some(_) => vm
        case _ => (context.getProgram.run compose program(vm.stack)) (vm) // Status is empty -> we can continue
      }
    }
  }

  def createProgramForMemory[A: Monoid](program: ProcessorF[A]): ProgramF[A] = context => new Program[A] {
    override def getProgram: Action[VirtualMachine[A]] = new Action[VirtualMachine[A]] {
      override val run: VirtualMachineF[A] = vm => vm.status match {
        case Some(_) => vm
        case _ => (context.getProgram.run compose program(vm.stack, vm.memory)) (vm) // Status is empty -> we can continue
      }
    }
  }

  def createIndexedProgram[A: Monoid](index: Int): ProcessorF[A] => ProgramF[A] = program => context => new Program[A] {
      override def getProgram: Action[VirtualMachine[A]] = new Action[VirtualMachine[A]] {
        override val run: VirtualMachineF[A] = vm => {
          if (index < 0 || index >= VirtualMachine.memorySize)
            VirtualMachine.error[A](s"index [$index] is out of bounds").apply(vm)
          else vm.status match {
            case Some(_) => vm
            case _ => (context.getProgram.run compose program(vm.stack, vm.memory)) (vm) // Status is empty -> we can continue
          }
        }
      }
    }

  implicit def programCompositionInstance[A: Monoid]: Monoid[ProgramF[A]] = new Monoid[ProgramF[A]] {
    override def empty: ProgramF[A] = idF

    override def combine(f: ProgramF[A], g: ProgramF[A]): ProgramF[A] = context => new Program[A] {
      override def getProgram: Action[VirtualMachine[A]] = new Action[VirtualMachine[A]] {
        override def run: VirtualMachineF[A] = g(context).getProgram.run compose f(context).getProgram.run
      }
    }
  }
}
