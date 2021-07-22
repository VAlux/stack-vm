package com.alvo

import cats.kernel.Monoid
import com.alvo.VirtualMachineAlg.{Memory, Processor, Stack}
import com.alvo.code.Term
import com.alvo.code.Term.*
import scala.language.implicitConversions

trait ProgramTF[F[_], J]:
  def getProgram: F[TermActionTF[F, J]]

case class TermActionTF[F[_], J](termList: List[Term] = NOP, action: Action[F[VirtualMachineAlg[F, J]]])

object ProgramTF:

  import cats.effect.kernel.Sync
  import cats.implicits.given
  import cats.syntax.all.{*, given}

  type SProcessor[F[_], J] = Stack => Processor[F, J]
  type SMProcessor[F[_], J] = (Stack, Memory) => Processor[F, J]

  def id[F[_], J: Monoid](using F: Sync[F]): ProgramTF[F, J] = new ProgramTF[F, J]:
    override def getProgram: F[TermActionTF[F, J]] = F.delay {
      TermActionTF(action =
        new Action[F[VirtualMachineAlg[F, J]]]:
          override def run: Processor[F, J] = identity
      )
    }

  def createProgramForStack[F[_], J: Monoid](terms: List[Term], processor: SProcessor[F, J], program: ProgramTF[F, J])(
    using F: Sync[F]
  ): ProgramTF[F, J] =
    new:
      override def getProgram: F[TermActionTF[F, J]] = F.delay {
        TermActionTF(
          terms,
          new Action[F[VirtualMachineAlg[F, J]]]:
            override val run: Processor[F, J] =
              vmF =>
                for
                  vm <- vmF
                  status <- vm.getStatus()
                  stack <- vm.getStack()
                  vmProgram <- program.getProgram
                  resVm <- status match
                    case Some(_) => vmF
                    case _       => processor(stack)(vmProgram.action.run(vmF))
                yield resVm
        )
      }

  def createProgramForMemory[F[_], J: Monoid](
    terms: List[Term],
    processor: SMProcessor[F, J],
    program: ProgramTF[F, J]
  )(using F: Sync[F]): ProgramTF[F, J] =
    new:
      override def getProgram: F[TermActionTF[F, J]] = F.delay {
        TermActionTF(
          terms,
          new Action[F[VirtualMachineAlg[F, J]]]:
            override val run: Processor[F, J] =
              vmF =>
                for
                  vm <- vmF
                  status <- vm.getStatus()
                  stack <- vm.getStack()
                  memory <- vm.getMemory()
                  vmProgram <- program.getProgram
                  resVm <- status match
                    case Some(_) => vmF
                    case _       => processor(stack, memory)(vmProgram.action.run(vmF))
                yield resVm
        )
      }

  def createIndexedProgramTF[F[_], J: Monoid](
    terms: List[Term],
    index: Int,
    processor: SMProcessor[F, J],
    program: ProgramTF[F, J]
  )(using F: Sync[F]): ProgramTF[F, J] =
    new:
      override def getProgram: F[TermActionTF[F, J]] = F.delay {
        TermActionTF(
          terms,
          new Action[F[VirtualMachineAlg[F, J]]]:
            override val run: Processor[F, J] = vmF =>
              if (index < 0 || index >= VirtualMachine.memorySize)
                VirtualMachineAlg.error[F, J](s"index [$index] is out of bounds", vmF)
              else
                for
                  vm <- vmF
                  status <- vm.getStatus()
                  stack <- vm.getStack()
                  memory <- vm.getMemory()
                  vmProgram <- program.getProgram
                  resVm <- status match
                    case Some(_) => vmF
                    case _       => processor(stack, memory)(vmProgram.action.run(vmF))
                yield resVm
        )
      }

// given programCompositionInstance[A: Monoid]: Monoid[ProgramF[A]] = new Monoid[ProgramF[A]]:
//   override def empty: ProgramF[A] = idF

//   override def combine(f: ProgramF[A], g: ProgramF[A]): ProgramF[A] = context =>
//     new ProgramTF[A]:
//       val terms: List[Term] = g(context).getProgram.termList ::: f(context).getProgram.termList
//       override def getProgram: TermActionTF[A] = TermActionTF(
//         terms,
//         new Action[VirtualMachine[A]]:
//           override def run: Processor[A] =
//             g(context).getProgram.action.run compose f(context).getProgram.action.run
//       )
