package com.alvo

import cats.kernel.Monoid
import cats.syntax.monoid.*
import com.alvo.Program.id
import com.alvo.VirtualMachine.*
import com.alvo.code.Term
import com.alvo.code.Term.*
import com.alvo.code.TermProgramIsomorphism.fromCode
import cats.effect.kernel.Sync
import cats.effect.kernel.Ref

trait VirtualMachineAlg[F[_], J, VM[F[_], J]]:
  def setStack(newStack: Stack): F[VM[F, J]]
  def setMemory(newMemory: Memory): F[VM[F, J]]
  def setStatus(newStatus: VMStatus): F[VM[F, J]]
  def addRecord(newRecord: J): F[VM[F, J]]

object VirtualMachineAlg:
  import cats.implicits.given

  def apply[F[_], J: Monoid](
    vm: VirtualMachineTF[F, J]
  )(using F: Sync[F]): F[VirtualMachineAlg[F, J, VirtualMachineTF]] = F.delay {
    new:
      override def setStack(newStack: Stack): F[VirtualMachineTF[F, J]] =
        vm.stack.update(_ => newStack).map(_ => vm)
      override def setMemory(newMemory: Memory): F[VirtualMachineTF[F, J]] =
        vm.memory.update(_ => newMemory).map(_ => vm)
      override def setStatus(newStatus: VMStatus): F[VirtualMachineTF[F, J]] =
        vm.status.update(_ => newStatus).map(_ => vm)
      override def addRecord(newRecord: J): F[VirtualMachineTF[F, J]] =
        vm.journal.update(currentJournal => newRecord |+| currentJournal).map(_ => vm)
  }

  def empty[F[_]: Sync, J: Monoid]: F[VirtualMachineAlg[F, J, VirtualMachineTF]] = for {
    stack <- Ref.of[F, Stack](List.empty)
    mem <- Ref.of[F, Memory](Array.fill(memorySize)(0))
    status <- Ref.of[F, VMStatus](None)
    journal <- Ref.of[F, J](Monoid.empty)
    vm <- VirtualMachineAlg(VirtualMachineTF(stack, mem, status, journal))
  } yield vm

end VirtualMachineAlg

case class VirtualMachineTF[F[_], J: Monoid](
  stack: Ref[F, Stack],
  memory: Ref[F, Memory],
  status: Ref[F, VMStatus],
  journal: Ref[F, J]
)

case class VirtualMachine[A: Monoid](stack: Stack, memory: Memory, status: VMStatus, journal: A):
  def setStack(newStack: Stack): VirtualMachine[A] = this.copy(stack = newStack)
  def setMemory(newMemory: Memory): VirtualMachine[A] = this.copy(memory = newMemory)
  def setStatus(newStatus: VMStatus): VirtualMachine[A] = this.copy(status = newStatus)
  def addRecord(newRecord: A): VirtualMachine[A] = this.copy(journal = newRecord |+| journal)

object VirtualMachine:
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

  def error[A: Monoid](message: String): Processor[A] =
    (vm: VirtualMachine[A]) => vm.setStatus(Some(s"Error: $message"))

  def memoryUpdate(memory: Memory, at: Int, value: Int): Memory =
    memory.update(at, value)
    memory

  extension (vm: VirtualMachine[_])
    def mkString: String =
      s"stack[: ${vm.stack mkString ":"}] | memory: [${vm.memory mkString " "}] | status: [${vm.status}]"

end VirtualMachine

object Bootstrap:
  import com.alvo.loggers.Logger.{given, *}
  import com.alvo.operations.CompositeOperations.{given, *}

  @main def entrypoint(args: String*): Unit =
    println("\nEvaluation started...")

    val resRange = execute(fromCode[CodeLog](PUSH(2) :: PUSH(6) :: range[CodeLog]))
    val resGcd = execute(fromCode[StackStateLog](PUSH(6) :: PUSH(9) :: gcd[StackStateLog]))
    val resFactIter = execute(fromCode[StackStateLog](PUSH(3) :: memFactIter[StackStateLog]))

    println("\nCalculating range")
    println(resRange.journal.reverse mkString "\n")
    println("\nResulting stack")
    println(resRange.stack)

    println("\nCalculating factorial hom")
    println(resFactIter.journal.reverse mkString "\n")
    println("\nResulting stack")
    println(resFactIter.stack)

    println("\nCalculating GCD")
    println(resGcd.journal.reverse mkString "\n")
    println("\nResulting stack")
    println(resGcd.stack)

    println("\nEvaluation finished. VM terminated")
