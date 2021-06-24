package com.alvo

import cats.kernel.Monoid
import cats.syntax.monoid.*
import com.alvo.Program.id
import com.alvo.VirtualMachine.*
import com.alvo.code.Terms.Term
import com.alvo.code.Terms.Term.*
import com.alvo.code.TermProgramIsomorphism.fromCode

case class VirtualMachine[A: Monoid](stack: Stack, memory: Memory, status: VMStatus, journal: A) {
  def setStack(newStack: Stack): VirtualMachine[A] = this.copy(stack = newStack)

  def setMemory(newMemory: Memory): VirtualMachine[A] = this.copy(memory = newMemory)

  def setStatus(newStatus: VMStatus): VirtualMachine[A] = this.copy(status = newStatus)

  def addRecord(newRecord: A): VirtualMachine[A] = this.copy(journal = newRecord |+| journal)
}

object VirtualMachine {

  type Stack = List[Int]
  type Memory = Array[Int]
  type Processor[A] = VirtualMachine[A] => VirtualMachine[A]
  type VMStatus = Option[String]
  type ProgramF[A] = Program[A] => Program[A]

  val memorySize = 4

  def emptyVM[A: Monoid]: VirtualMachine[A] =
    VirtualMachine[A](List.empty, Array.fill(memorySize)(0), None, Monoid.empty[A])

  def run[A: Monoid](program: Program[A]): Processor[A] = program.getProgram.action.run

  def execute[A: Monoid](program: ProgramF[A]): VirtualMachine[A] =
    run[A](program(id)).apply(emptyVM)

//  def executeLog[A: Monoid](program: ProgramF[A])(implicit context: TermList => Program[A]): VirtualMachine[A] ={
//    run[A](program(context)).apply(emptyVM)
//  }

  def error[A: Monoid](message: String): Processor[A] = {
    (vm: VirtualMachine[A]) => vm.setStatus(Some(s"Error: $message"))
  }

  def memoryUpdate(memory: Memory, at: Int, value: Int): Memory = {
    memory.update(at, value)
    memory
  }

  implicit class VirtualMachineSyntax(val vm: VirtualMachine[_]) extends AnyVal {
    def mkString: String =
      s"stack[: ${vm.stack mkString ":"}] | memory: [${vm.memory mkString " "}] | status: [${vm.status}]"
  }
}

object Bootstrap {

  import com.alvo.loggers.Logger.{given, *}
  import com.alvo.operations.CompositeOperations.{given, *}

  def main(args: Array[String]): Unit = {

    println("\nEvaluation started...")

    type Debug = CodeLog

    val resRange = execute(fromCode[Debug](PUSH(2) :: PUSH(6) :: range[Debug]))

    val resGcd = execute(fromCode[StackStateLog](PUSH(6) :: PUSH(9) :: gcd[StackStateLog]))

    val resFactIter = execute(fromCode[StackStateLog](PUSH(3) :: memFactIter[StackStateLog]))

    println("\nCalculating range")
    println(resRange.journal.reverse mkString "\n")

    println("\nCalculating factorial hom")
    println(resFactIter.journal.reverse mkString "\n")

    println("\nCalculating GCD")
    println(resGcd.journal.reverse mkString "\n")

    println("\nEvaluation finished. VM terminated")
  }
}
