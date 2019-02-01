package com.alvo

import cats.kernel.{Monoid, Semigroup}
import cats.syntax.monoid._
import com.alvo.VirtualMachine._

import scala.annotation.tailrec

final case class VirtualMachine[A: Monoid](stack: Stack, memory: Memory, status: VMStatus, journal: A) {
  def setStack(newStack: Stack): VirtualMachine[A] = this.copy(stack = newStack)

  def setMemory(newMemory: Memory): VirtualMachine[A] = this.copy(memory = newMemory)

  def setStatus(newStatus: VMStatus): VirtualMachine[A] = this.copy(status = newStatus)

  def addRecord(record: A): VirtualMachine[A] = this.copy(journal = journal |+| record)
}

object VirtualMachine {

  type Stack = List[Int]
  type Memory = Array[Int]
  type Processor[A] = VirtualMachine[A] => VirtualMachine[A]
  type VMStatus = Option[String]
  type Operation[A] = Program[A] => Program[A]

  val memorySize = 4

  def emptyVM[A: Monoid]: VirtualMachine[A] =
    VirtualMachine[A](List.empty, Array.fill(memorySize)(0), None, Monoid.empty[A])

  def run[A]: Program[A] => Processor[A] = _.getProgram.run

  def execute[A: Monoid](program: Program[A]): VirtualMachine[A] = run(program)(emptyVM[A])

  def error[A]: String => Processor[A] = message => {
    vm: VirtualMachine[A] => vm.setStatus(Some(s"Error: $message"))
  }

  private def updateMemory(memory: Memory, at: Int, value: Int): Memory = {
    memory.update(at, value)
    memory
  }

  def pop[A]: Program[A] => Program[A] = Program.createProgramForStack[A] { stack =>
    vm =>
      stack match {
        case _ :: xs => vm.setStack(xs)
        case _ => error("pop action expected an argument")(vm)
      }
  }

  def push[A]: Int => Program[A] => Program[A] = element => Program.createProgramForStack[A] { stack =>
    vm =>
      vm.setStack(element :: stack)
  }

  def dup[A]: Program[A] => Program[A] = Program.createProgramForStack[A] { stack =>
    vm =>
      stack match {
        case x :: xs => vm.setStack(x :: x :: xs)
        case _ => error("dup action expects an argument")(vm)
      }
  }

  def swap[A]: Program[A] => Program[A] = Program.createProgramForStack[A] { stack =>
    vm =>
      stack match {
        case x :: y :: xs => vm.setStack(y :: x :: xs)
        case _ => error("swap action expects 2 arguments")(vm)
      }
  }

  def exch[A]: Program[A] => Program[A] = Program.createProgramForStack[A] { stack =>
    vm =>
      stack match {
        case x :: y :: xs => vm.setStack(y :: x :: y :: xs)
        case _ => error("exch action expects 2 arguments")(vm)
      }
  }

  def put[A]: Int => Program[A] => Program[A] = index => Program.createIndexedArgumentProgram[A](index) { (stack, memory) =>
    vm =>
      stack match {
        case x :: xs => vm.setStack(xs).setMemory(updateMemory(memory, index, x))
        case _ => error("put action expects an argument")(vm)
      }
  }

  def get[A]: Int => Program[A] => Program[A] = index =>
    Program.createIndexedArgumentProgram[A](index) { (stack, memory) =>
      vm => vm.setStack(memory(index) :: stack)
    }

  def unary[A]: (String, Int => Stack) => Program[A] => Program[A] = (name, operation) => Program.createProgramForStack[A] { stack =>
    vm =>
      stack match {
        case x :: xs => vm.setStack(operation(x) ++ xs)
        case _ => error(s"operation $name expected an argument")(vm)
      }
  }

  def binary[A]: (String, Int => Int => Stack) => Program[A] => Program[A] = (name, operation) =>
    Program.createProgramForStack[A] { stack =>
      vm =>
        stack match {
          case x :: y :: xs => vm.setStack(operation(x)(y) ++ xs)
          case _ => error(s"operation $name expected 2 arguments")(vm)
        }
    }

  def neg[A]: Program[A] => Program[A] = unary[A]("neg", a => -a :: Nil)

  def inc[A]: Program[A] => Program[A] = unary[A]("inc", a => (a + 1) :: Nil)

  def dec[A]: Program[A] => Program[A] = unary[A]("dec", a => (a - 1) :: Nil)

  def add[A]: Program[A] => Program[A] = binary[A]("add", a => b => (a + b) :: Nil)

  def sub[A]: Program[A] => Program[A] = binary[A]("sub", a => b => (b - a) :: Nil)

  def mul[A]: Program[A] => Program[A] = binary[A]("mul", a => b => (a * b) :: Nil)

  def div[A]: Program[A] => Program[A] = binary[A]("div", a => b => (b / a) :: Nil)

  def eqv[A]: Program[A] => Program[A] = binary[A]("eq", a => b => if (a == b) 1 :: Nil else 0 :: Nil)

  def lte[A]: Program[A] => Program[A] = binary[A]("lt", a => b => if (a > b) 1 :: Nil else 0 :: Nil)

  def gte[A]: Program[A] => Program[A] = binary[A]("gt", a => b => if (a < b) 1 :: Nil else 0 :: Nil)

  def neq[A]: Program[A] => Program[A] = binary[A]("neq", a => b => if (a != b) 1 :: Nil else 0 :: Nil)

  def mod[A]: Program[A] => Program[A] = binary[A]("mod", a => b => (b % a) :: Nil)

  def proceed[A]: Program[A] => Program[A] => Stack => Processor[A] =
    pr => program => stack => (pr.getProgram.run compose program.getProgram.run) compose { vm => vm.setStack(stack) }

  def branch[A]: Program[A] => Program[A] => Program[A] => Program[A] => Program[A] =
    branch1 => branch2 => program => Program.createProgramForStack[A] { stack =>
      vm =>
        stack match {
          case x :: xs => proceed(if (x != 0) branch1 else branch2)(program)(xs)(vm)
          case _ => error("branch requires an argument")(vm)
        }
    }

  def rep[A]: Operation[A] => Operation[A] = body => program =>
    Program.createProgramForStack[A] { stack =>
      vm =>
        stack match {
          case x :: xs => proceed[A](Semigroup[Program[A]].combineN(body, x))(program)(xs)(vm)
          case _ => error("rep operation required an argument")(vm)
        }
    }

  def loop[A]: Program[A] => Program[A] => Program[A] => Program[A] => Program[A] = test => body => program =>
    Program.createProgramForStack[A] { _ =>
      vm => {
        @tailrec
        def iterate(machine: VirtualMachine[A]): VirtualMachine[A] = {
          val testResult = proceed(test)(program)(machine.stack)(machine)
          testResult.stack match {
            case 0 :: xs => proceed[A](Program.id)(program)(xs)(machine)
            case _ :: xs => iterate(proceed[A](body)(program)(xs)(machine))
            case _ => error("while operation required an argument")(machine)
          }
        }

        iterate(vm)
      }
    }

  implicit class VirtualMachineSyntax(val vm: VirtualMachine[_]) extends AnyVal {
    def mkString: String =
      s"stack: ${vm.stack mkString " - "} || memory: ${vm.memory mkString " "} || status: ${vm.status}"
  }

}

object Bootstrap {

  def range[A]: Operation[A] =
    exch[A] |+| sub[A] |+| rep[A](dup[A] |+| inc[A])

  //TODO: recursion is not supported yet
  //  def recursiveFact: Program =
  //    dup |+| push(2) |+| lte |+| branch(push(1))(dup |+| dec |+| recursiveFact) |+| mul
  //
  //  val memFactIter: Program =
  //    dup |+| put(0) |+| dup |+| dec |+| rep(dec |+| dup |+| get(0) |+| mul |+| put(0)) |+| get(0) |+| swap |+| pop
  //
    def copy[A]: Operation[A] = exch[A] |+| exch[A]
  //
//    def gcd[A]: Program[A] => Program[A]=
//      loop[A](copy[A] |+| neq[A])(copy |+| lte |+| branch(Program.id)(swap) |+| exch |+| sub) |+| pop


  def main(args: Array[String]): Unit = {
    def resRange[A]= execute[A](push[A](2) |+| push[A](6) |+| range[A])()

    //    val resGcd = execute(push(6) |+| push(9) |+| gcd)
    //    val resFactMemIter = execute(push(6) |+| memFactIter)
    println(resRange mkString)
    println("Evaluation finished. VM terminated")
  }
}

