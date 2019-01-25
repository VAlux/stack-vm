package com.alvo

import com.alvo.VirtualMachine.{Memory, Stack, VMStatus}

case class VirtualMachine(stack: Stack, memory: Memory, status: VMStatus) {
  def setStack(newStack: Stack): VirtualMachine = this.copy(stack = newStack)

  def setMemory(newMemory: Memory): VirtualMachine = this.copy(memory = newMemory)

  def setStatus(newStatus: VMStatus): VirtualMachine = this.copy(status = newStatus)
}

object VirtualMachine {

  type Stack = List[Int]
  type Memory = Array[Int]
  type Processor = VirtualMachine => VirtualMachine
  type VMStatus = Option[String]

  val memorySize = 4

  val empty: VirtualMachine = VirtualMachine(List.empty, Array.fill(memorySize)(0), None)

  val run: Program => Processor = program => program.getProgram.run

  val execute: Program => VirtualMachine = program => run(program)(empty)

  val error: String => Processor = message => {
    vm: VirtualMachine => vm.setStatus(Some(s"Error: $message"))
  }

  val updateMemory: (Memory, Int, Int) => Memory = { (mem, at, value) =>
    mem.update(at, value)
    mem
  }

  val pop: Program = Program.createProgramForStack { stack =>
    vm =>
      stack match {
        case _ :: xs => vm.setStack(xs)
        case _ => error("pop action expected an argument")(vm)
      }
  }

  val push: Int => Program = element => Program.createProgramForStack { stack =>
    vm =>
      vm.setStack(stack :+ element)
  }

  val dup: Program = Program.createProgramForStack { stack =>
    vm =>
      stack match {
        case x :: xs => vm.setStack(x :: x :: xs)
        case _ => error("dup action expects an argument")(vm)
      }
  }

  val swap: Program = Program.createProgramForStack { stack =>
    vm =>
      stack match {
        case x :: y :: xs => vm.setStack(y :: x :: xs)
        case _ => error("swap action expects 2 arguments")(vm)
      }
  }

  val exch: Program = Program.createProgramForStack { stack =>
    vm =>
      stack match {
        case x :: y :: xs => vm.setStack(y :: x :: y :: xs)
        case _ => error("exch action expects 2 arguments")(vm)
      }
  }

  val put: Int => Program = index => Program.createIndexedArgumentProgram(index) { (stack, memory) =>
    vm =>
      stack match {
        case x :: xs => vm.setStack(xs).setMemory(updateMemory(memory, index, x))
        case _ => error("put action expects an argument")(vm)
      }
  }
}

