package com.alvo

import com.alvo.VirtualMachine.{Memory, Processor, Stack}

trait Program {
  def getProgram: Action[VirtualMachine]
}

object Program {
  val createProgramForStack: (Stack => Processor) => Program = f => new Program {
    override def getProgram: Action[VirtualMachine] = new Action[VirtualMachine] {
      override val run: VirtualMachine => VirtualMachine = vm => vm.status match {
        case Some(_) => vm
        case _ => f(vm.stack)(vm) // Status is empty -> we can continue
      }
    }
  }

  val createProgramForMemory: ((Stack, Memory) => Processor) => Program = f => new Program {
    override def getProgram: Action[VirtualMachine] = new Action[VirtualMachine] {
      override val run: VirtualMachine => VirtualMachine = vm => vm.status match {
        case Some(_) => vm
        case _ => f(vm.stack, vm.memory)(vm) // Status is empty -> we can continue
      }
    }
  }

  val createIndexedArgumentProgram: Int => ((Stack, Memory) => Processor) => Program = index => ƒ =>
    createProgramForMemory { (stack, memory) =>
      vm =>
        if (index < 0 || index >= VirtualMachine.memorySize) VirtualMachine.error(s"index [$index] is out of bounds")(vm)
        else ƒ(stack, memory)(vm)
    }
}
