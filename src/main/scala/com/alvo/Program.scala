package com.alvo

import cats.kernel.Monoid
import com.alvo.VirtualMachine.{Memory, Processor, Stack}

trait Program {
  def getProgram: Action[VirtualMachine]
}

object Program {

  val id: Program = new Program {
    override def getProgram: Action[VirtualMachine] = new Action[VirtualMachine] {
      override def run: VirtualMachine => VirtualMachine = identity
    }
  }

  val createProgramForStack: (Stack => Processor) => Program = func => new Program {
    override def getProgram: Action[VirtualMachine] = new Action[VirtualMachine] {
      override val run: VirtualMachine => VirtualMachine = vm => vm.status match {
        case Some(_) => vm
        case _ => func(vm.stack)(vm) // Status is empty -> we can continue
      }
    }
  }

  val createProgramForMemory: ((Stack, Memory) => Processor) => Program = func => new Program {
    override def getProgram: Action[VirtualMachine] = new Action[VirtualMachine] {
      override val run: VirtualMachine => VirtualMachine = vm => vm.status match {
        case Some(_) => vm
        case _ => func(vm.stack, vm.memory)(vm) // Status is empty -> we can continue
      }
    }
  }

  implicit val programCompositionInstance: Monoid[Program] = new Monoid[Program] {
    override def empty: Program = id

    override def combine(x: Program, y: Program): Program = new Program {
      override def getProgram: Action[VirtualMachine] = new Action[VirtualMachine] {
        override def run: VirtualMachine => VirtualMachine = y.getProgram.run compose x.getProgram.run
      }
    }
  }

  val createIndexedArgumentProgram: Int => ((Stack, Memory) => Processor) => Program = index => func =>
    createProgramForMemory { (stack, memory) =>
      vm =>
        if (index < 0 || index >= VirtualMachine.memorySize)
          VirtualMachine.error(s"index [$index] is out of bounds")(vm)
        else
          func(stack, memory)(vm)
    }
}
