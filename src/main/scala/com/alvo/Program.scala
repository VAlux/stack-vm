package com.alvo

import cats.kernel.Monoid
import com.alvo.VirtualMachine.{Memory, Processor, Stack}

trait Program[A] {
  def getProgram: Action[VirtualMachine[A]]
}

object Program {

  def id[A]: Program[A] = new Program[A] {
    override def getProgram: Action[VirtualMachine[A]] = new Action[VirtualMachine[A]] {
      override val run: VirtualMachine[A] => VirtualMachine[A] = identity
    }
  }

  def createProgramForStack[A]: (Stack => Processor[A]) => Program[A] => Program[A] =
    func => program => new Program[A] {
      override def getProgram: Action[VirtualMachine[A]] = new Action[VirtualMachine[A]] {
        override val run: VirtualMachine[A] => VirtualMachine[A] = vm => vm.status match {
          case Some(_) => vm
          case _ => (program.getProgram.run compose func(vm.stack)) (vm) // Status is empty -> we can continue
        }
      }
    }

  def createProgramForMemory[A]: ((Stack, Memory) => Processor[A]) => Program[A] => Program[A] =
    func => program => new Program[A] {
      override def getProgram: Action[VirtualMachine[A]] = new Action[VirtualMachine[A]] {
        override val run: VirtualMachine[A] => VirtualMachine[A] = vm => vm.status match {
          case Some(_) => vm
          case _ => (program.getProgram.run compose func(vm.stack, vm.memory)) (vm) // Status is empty -> we can continue
        }
      }
    }

  implicit def programCompositionInstance[A]: Monoid[Program[A] => Program[A]] = new Monoid[Program[A] => Program[A]] {
    override def empty: Program[A] => Program[A] = id[A]()

    override def combine(x: Program[A] => Program[A], y: Program[A] => Program[A]): Program[A] => Program[A] = {
      y compose x
    }
  }

  implicit def programCompositionInstance[A]: Monoid[Program[A]] = new Monoid[Program[A]] {
    override def empty: Program[A] = id

    override def combine(x: Program[A], y: Program[A]): Program[A] = new Program[A] {
      override def getProgram: Action[VirtualMachine[A]] = new Action[VirtualMachine[A]] {
        override val run: VirtualMachine[A] => VirtualMachine[A] = y.getProgram.run compose x.getProgram.run
      }
    }
  }

  def createIndexedArgumentProgram[A]: Int => ((Stack, Memory) => Processor[A]) => Program[A] => Program[A] =
    index => func =>
      createProgramForMemory[A] { (stack, memory) =>
        vm =>
          if (index < 0 || index >= VirtualMachine.memorySize)
            VirtualMachine.error(s"index [$index] is out of bounds")(vm)
          else
            func(stack, memory)(vm)
      }
}
