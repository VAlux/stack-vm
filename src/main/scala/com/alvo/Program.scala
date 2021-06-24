package com.alvo

import cats.kernel.Monoid
import com.alvo.VirtualMachine.{Memory, Processor, ProgramF, Stack}
import com.alvo.code.Terms.Term
import com.alvo.code.Terms.Term.*
import scala.language.implicitConversions

trait Program[A]:
  def getProgram: TermAction[A]

case class TermAction[A](action: Action[VirtualMachine[A]], termList: List[Term] = NOP)

object Program:

  type VirtualMachineF[A] = VirtualMachine[A] => VirtualMachine[A]

  type ProcessorF[A] = (Stack, Memory) => Processor[A]

  def id[A: Monoid]: Program[A] = new Program[A] {
    override def getProgram: TermAction[A] = TermAction(new Action[VirtualMachine[A]] {
      override def run: VirtualMachineF[A] = identity
    })
  }

  def idF[A: Monoid]: ProgramF[A] = _ => Program.id

  def createProgramForStack[A: Monoid](terms: List[Term])(program: Stack => Processor[A]): ProgramF[A] = context => new Program[A] {
    override def getProgram: TermAction[A] = TermAction(new Action[VirtualMachine[A]] {
      override val run: VirtualMachineF[A] = vm => vm.status match {
        case Some(_) => vm
        case _ => (context.getProgram.action.run compose program(vm.stack)) (vm) // Status is empty -> we can continue
      }
    }, terms)
  }

  def createProgramForMemory[A: Monoid](terms: List[Term])(program: ProcessorF[A]): ProgramF[A] = context => new Program[A] {
    override def getProgram: TermAction[A] = TermAction(new Action[VirtualMachine[A]] {
      override val run: VirtualMachineF[A] = vm => vm.status match {
        case Some(_) => vm
        case _ => (context.getProgram.action.run compose program(vm.stack, vm.memory)) (vm) // Status is empty -> we can continue
      }
    }, terms)
  }

  def createIndexedProgram[A: Monoid](terms: List[Term])(index: Int): ProcessorF[A] => ProgramF[A] = program => context => new Program[A] {
    override def getProgram: TermAction[A] = TermAction(new Action[VirtualMachine[A]] {
      override val run: VirtualMachineF[A] = vm => {
        if (index < 0 || index >= VirtualMachine.memorySize)
          VirtualMachine.error[A](s"index [$index] is out of bounds").apply(vm)
        else vm.status match
          case Some(_) => vm
          case _ => (context.getProgram.action.run compose program(vm.stack, vm.memory)) (vm) // Status is empty -> we can continue
      }
    }, terms)
  }

  implicit def programCompositionInstance[A: Monoid]: Monoid[ProgramF[A]] = new Monoid[ProgramF[A]] {
    override def empty: ProgramF[A] = idF

    override def combine(f: ProgramF[A], g: ProgramF[A]): ProgramF[A] = context => new Program[A] {
      val terms: List[Term] = g(context).getProgram.termList ::: f(context).getProgram.termList
      override def getProgram: TermAction[A] = TermAction(new Action[VirtualMachine[A]] {
        override def run: VirtualMachineF[A] = g(context).getProgram.action.run compose f(context).getProgram.action.run
      }, terms)
    }
  }
