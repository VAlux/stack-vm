package com.alvo

import cats.kernel.Monoid
import com.alvo.VirtualMachine.{Memory, Processor, ProgramF, Stack}
import com.alvo.code.Term
import com.alvo.code.Term.*
import scala.language.implicitConversions

trait Program[A]:
  def getProgram: TermAction[A]

case class TermAction[A](termList: List[Term] = NOP, action: Action[VirtualMachine[A]])

object Program:

  type SProcessor[A] = Stack => Processor[A]
  type SMProcessor[A] = (Stack, Memory) => Processor[A]

  def id[A: Monoid]: Program[A] = new Program[A]:
    override def getProgram: TermAction[A] = TermAction(action =
      new Action[VirtualMachine[A]]:
        override def run: Processor[A] = identity
    )

  def idF[A: Monoid]: ProgramF[A] = _ => Program.id

  def createProgramForStack[A: Monoid](terms: List[Term])(program: SProcessor[A]): ProgramF[A] = context =>
    new Program[A]:
      override def getProgram: TermAction[A] = TermAction(
        terms,
        new Action[VirtualMachine[A]]:
          override val run: Processor[A] = vm =>
            vm.status match
              case Some(_) => vm
              case _       => (context.getProgram.action.run compose program(vm.stack))(vm)
      )

  def createProgramForMemory[A: Monoid](terms: List[Term])(program: SMProcessor[A]): ProgramF[A] = context =>
    new Program[A]:
      override def getProgram: TermAction[A] = TermAction(
        terms,
        new Action[VirtualMachine[A]]:
          override val run: Processor[A] = vm =>
            vm.status match
              case Some(_) => vm
              case _       => (context.getProgram.action.run compose program(vm.stack, vm.memory))(vm)
      )

  def createIndexedProgram[A: Monoid](terms: List[Term])(index: Int): SMProcessor[A] => ProgramF[A] = program =>
    context =>
      new Program[A]:
        override def getProgram: TermAction[A] = TermAction(
          terms,
          new Action[VirtualMachine[A]]:
            override val run: Processor[A] = vm =>
              if (index < 0 || index >= VirtualMachine.memorySize)
                VirtualMachine.error[A](s"index [$index] is out of bounds").apply(vm)
              else
                vm.status match
                  case Some(_) => vm
                  case _       => (context.getProgram.action.run compose program(vm.stack, vm.memory))(vm)
        )

  given programCompositionInstance[A: Monoid]: Monoid[ProgramF[A]] = new Monoid[ProgramF[A]]:
    override def empty: ProgramF[A] = idF

    override def combine(f: ProgramF[A], g: ProgramF[A]): ProgramF[A] = context =>
      new Program[A]:
        val terms: List[Term] = g(context).getProgram.termList ::: f(context).getProgram.termList
        override def getProgram: TermAction[A] = TermAction(
          terms,
          new Action[VirtualMachine[A]]:
            override def run: Processor[A] =
              g(context).getProgram.action.run compose f(context).getProgram.action.run
        )
