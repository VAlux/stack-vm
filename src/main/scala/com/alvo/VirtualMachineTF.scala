package com.alvo

import cats.kernel.Monoid
import cats.syntax.monoid.*
import cats.effect.kernel.Sync
import cats.effect.kernel.Ref
import VirtualMachineAlg.*

trait VirtualMachineAlg[F[_], J]:
  def setStack(newStack: Stack): F[VirtualMachineAlg[F, J]]
  def getStack(): F[Stack]
  def setMemory(newMemory: Memory): F[VirtualMachineAlg[F, J]]
  def getMemory(): F[Memory]
  def setStatus(newStatus: VMStatus): F[VirtualMachineAlg[F, J]]
  def getStatus(): F[VMStatus]
  def addRecord(newRecord: J): F[VirtualMachineAlg[F, J]]

object VirtualMachineAlg:
  import cats.implicits.given
  import cats.syntax.all.{*, given}

  type Stack = List[Int]
  type Memory = Array[Int]
  type Processor[F[_], J] = F[VirtualMachineAlg[F, J]] => F[VirtualMachineAlg[F, J]]
  type VMStatus = Option[String]

  def apply[F[_], J: Monoid](
    stack: Ref[F, Stack],
    memory: Ref[F, Memory],
    status: Ref[F, VMStatus],
    journal: Ref[F, J]
  )(using F: Sync[F]): F[VirtualMachineAlg[F, J]] = F.delay {
    new:
      override def setStack(newStack: Stack): F[VirtualMachineAlg[F, J]] =
        stack.update(_ => newStack).map(_ => this)
      override def getStack(): F[Stack] =
        stack.get
      override def setMemory(newMemory: Memory): F[VirtualMachineAlg[F, J]] =
        memory.update(_ => newMemory).map(_ => this)
      override def getMemory(): F[Memory] = memory.get
      override def setStatus(newStatus: VMStatus): F[VirtualMachineAlg[F, J]] =
        status.update(_ => newStatus).map(_ => this)
      override def getStatus(): F[VMStatus] = status.get
      override def addRecord(newRecord: J): F[VirtualMachineAlg[F, J]] =
        journal
          .update(currentJournal => newRecord |+| currentJournal)
          .map(_ => this)
  }

  def empty[F[_]: Sync, J: Monoid](memorySize: Int): F[VirtualMachineAlg[F, J]] =
    for
      stack <- Ref.of[F, Stack](List.empty)
      mem <- Ref.of[F, Memory](Array.fill(memorySize)(0))
      status <- Ref.of[F, VMStatus](None)
      journal <- Ref.of[F, J](Monoid.empty)
      vm <- VirtualMachineAlg(stack, mem, status, journal)
    yield vm

// def run[F[_], J: Monoid](program: Program[F]): Processor[F, J] = program.getProgram.action.run

  def error[F[_]: Sync, J: Monoid](message: String, vm: F[VirtualMachineAlg[F, J]]): F[VirtualMachineAlg[F, J]] =
    vm.flatMap(_.setStatus(Some(s"Error: $message")))
