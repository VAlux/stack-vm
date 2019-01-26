package com.alvo

import cats.kernel.Semigroup
import com.alvo.VirtualMachine._

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

  val run: Program => Processor = _.getProgram.run

  val execute: Program => VirtualMachine = program => run(program)(empty)

  val error: String => Processor = message => {
    vm: VirtualMachine => vm.setStatus(Some(s"Error: $message"))
  }

  private def updateMemory(memory: Memory, at: Int, value: Int): Memory = {
    memory.update(at, value)
    memory
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

  val get: Int => Program = index => Program.createIndexedArgumentProgram(index) { (stack, memory) =>
    vm => vm.setStack(memory(index) :: stack)
  }

  val unary: (String, Int => Stack) => Program = (name, operation) => Program.createProgramForStack { stack =>
    vm =>
      stack match {
        case x :: xs => vm.setStack(operation(x) ++ xs)
        case _ => error(s"operation $name expected an argument")(vm)
      }
  }

  val binary: (String, Int => Int => Stack) => Program = (name, operation) => Program.createProgramForStack { stack =>
    vm =>
      stack match {
        case x :: y :: xs => vm.setStack(operation(x)(y) ++ xs)
        case _ => error(s"operation $name expected 2 arguments")(vm)
      }
  }

  val neg: Program = unary("neg", a => -a :: Nil)
  val inc: Program = unary("inc", a => (a + 1) :: Nil)
  val dec: Program = unary("dec", a => (a - 1) :: Nil)
  val add: Program = binary("add", a => b => (a + b) :: Nil)
  val sub: Program = binary("sub", a => b => (a - b) :: Nil)
  val mul: Program = binary("mul", a => b => (a * b) :: Nil)
  val div: Program = binary("div", a => b => (a / b) :: Nil)
  val eq: Program = binary("eq", a => b => if (a == b) 1 :: Nil else -1 :: Nil)
  val lt: Program = binary("lt", a => b => if (a < b) 1 :: Nil else -1 :: Nil)
  val gt: Program = binary("gt", a => b => if (a > b) 1 :: Nil else -1 :: Nil)
  val neq: Program = binary("neq", a => b => if (a != b) 1 :: Nil else -1 :: Nil)
  val mod: Program = binary("mod", a => b => (a % b) :: Nil)

  val proceed: Program => Stack => Processor =
    program => stack => program.getProgram.run compose { vm => vm.setStack(stack) }

  val branch: Program => Program => Program =
    branch1 => branch2 => Program.createProgramForStack { stack =>
      vm =>
        stack match {
          case x :: xs => proceed(if (x != 0) branch1 else branch2)(xs)(vm)
          case _ => error("branch requires an argument")(vm)
        }
    }

  //  val loop: Program => Program => Program =
  //    testExpr => body => Program.createProgramForStack { stack =>
  //      vm => {
  //        lazy val res: VirtualMachine = proceed(testExpr)(vm.stack)(vm)
  //        stack match {
  //          ???
  //        }
  //      }
  //    }

  val rep: Program => Program = body => Program.createProgramForStack { stack =>
    vm =>
      stack match {
        case x :: xs => proceed(Semigroup[Program].combineN(body, x))(xs)(vm)
        case _ => error("rep operation required an argument")(vm)
      }
  }


  implicit class VirtualMachineSyntax(vm: VirtualMachine) {
    def mkString: String =
      s"stack: ${vm.stack} | memory: ${vm.memory mkString " "} | status: ${vm.status}"
  }

}

object Bootstrap {

  import VirtualMachine.VirtualMachineSyntax
  import cats.syntax.monoid._

  val fact: Program = dup |+| push(2) |+| lt |+|
    branch(push(1))(dup |+| dec |+| fact) |+|
    mul

  def main(args: Array[String]): Unit = {
    val res: VirtualMachine = execute(push(10) |+| fact)
    println(res mkString)
  }
}

