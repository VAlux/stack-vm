package com.alvo

import cats.kernel.Monoid
import cats.syntax.monoid._
import com.alvo.Program.{id, idF}
import com.alvo.VirtualMachine._

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

  def run[A: Monoid](program: Program[A]): Processor[A] = program.getProgram.run

  def execute[A: Monoid](program: ProgramF[A]): VirtualMachine[A] =
    run[A](program(id)).apply(emptyVM)

  def executeLog[A: Monoid](program: ProgramF[A])(implicit logger: VirtualMachine[A] => A): VirtualMachine[A] =
    run[A](program(log)).apply(emptyVM)

  private def log[A: Monoid](implicit applyRecord: VirtualMachine[A] => A): Program[A] = {
    new Program[A] {
      override def getProgram: Action[VirtualMachine[A]] = new Action[VirtualMachine[A]] {
        override def run: VirtualMachine[A] => VirtualMachine[A] =
          vm => vm.addRecord(applyRecord(vm))
      }
    }
  }

  def error[A: Monoid](message: String): Processor[A] = {
    vm: VirtualMachine[A] => vm.setStatus(Some(s"Error: $message"))
  }

  def memoryUpdate(memory: Memory, at: Int, value: Int): Memory = {
    memory.update(at, value)
    memory
  }

  implicit class VirtualMachineSyntax(val vm: VirtualMachine[_]) extends AnyVal {
    def mkString: String =
      s"stack: ${vm.stack mkString ":"} | memory: ${vm.memory mkString " "} | status: ${vm.status}"
  }

}

object Bootstrap {

  import com.alvo.operations.ArithmeticOperations._
  import com.alvo.operations.StackOperations._

  def range[A: Monoid]: ProgramF[A] =
    exch |+|
      sub |+|
      rep(
        dup |+|
          inc)

  def memFactIter[A: Monoid]: ProgramF[A] =
    dup |+|
      put() |+|
      dup |+|
      dec |+|
      rep(
        dec |+|
          dup |+|
          get() |+|
          mul |+|
          put()
      ) |+|
      get() |+|
      swap |+|
      pop

  def copy[A: Monoid]: ProgramF[A] =
    exch |+|
      exch

  def gcd[A: Monoid]: ProgramF[A] =
    loop(
      copy |+|
        neq
    )(
      copy |+|
        lte |+|
        branch(idF)(swap) |+|
        exch |+|
        sub
    ) |+|
      pop

  def main(args: Array[String]): Unit = {
    import com.alvo.loggers.CompositeLogger._
    import com.alvo.loggers.ComputationStepsAmountLogger._
    import com.alvo.loggers.Logger._
    import com.alvo.loggers.MemoryStateLogger._
    import com.alvo.loggers.StackStateLogger._

    println("\nEvaluation started...")

    type StackMemoryAndStepsLog = ((StackStateLog, MemoryStateLog), StepsAmountLog)

    lazy val resRange = executeLog(push[StackMemoryAndStepsLog](2) |+| push(6) |+| range)

    lazy val resGcd = executeLog(push[StackStateLog](6) |+| push(9) |+| gcd)

    lazy val resFactMemIter = executeLog(push[StackStateLog](11) |+| memFactIter)

    println("\nCreate range")
    println(resRange.journal match { case ((stack, memory), steps) => s"$stack\n${memory.map(m => m mkString " ")}\n$steps" })

    println("\nCalculating factorial")
    println(resFactMemIter.journal.reverse mkString "\n")

    println("\nCalculating GCD")
    println(resGcd.journal.reverse mkString "\n")

    println("\nEvaluation finished. VM terminated")
  }
}
