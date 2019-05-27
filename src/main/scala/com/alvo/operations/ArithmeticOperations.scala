package com.alvo.operations

import cats.kernel.Monoid
import com.alvo.Program
import com.alvo.VirtualMachine.{ProgramF, Stack, error}

object ArithmeticOperations {

  private[this] implicit val intToStack: Int => Stack = _ :: Nil

  private def unary[A: Monoid](name: String, operation: Int => Stack): ProgramF[A] =
    Program.createProgramForStack[A] { stack =>
      vm =>
        stack match {
          case x :: xs => vm.setStack(operation(x) ++ xs)
          case _ => error(s"operation $name expected an argument").apply(vm)
        }
    }

  private def binary[A: Monoid](name: String, operation: Int => Int => Stack): ProgramF[A] =
    Program.createProgramForStack[A] { stack =>
      vm =>
        stack match {
          case x :: y :: xs => vm.setStack(operation(x)(y) ++ xs)
          case _ => error(s"operation $name expected 2 arguments").apply(vm)
        }
    }

  def neg[A: Monoid]: ProgramF[A] = unary[A]("neg", a => -a)

  def inc[A: Monoid]: ProgramF[A] = unary[A]("inc", a => a + 1)

  def dec[A: Monoid]: ProgramF[A] = unary[A]("dec", a => a - 1)

  def add[A: Monoid]: ProgramF[A] = binary[A]("add", a => b => a + b)

  def sub[A: Monoid]: ProgramF[A] = binary[A]("sub", a => b => b - a)

  def mul[A: Monoid]: ProgramF[A] = binary[A]("mul", a => b => a * b)

  def div[A: Monoid]: ProgramF[A] = binary[A]("div", a => b => b / a)

  def eqv[A: Monoid]: ProgramF[A] = binary[A]("eq", a => b => if (a == b) 1 else 0)

  def lte[A: Monoid]: ProgramF[A] = binary[A]("lt", a => b => if (a > b) 1 else 0)

  def gte[A: Monoid]: ProgramF[A] = binary[A]("gt", a => b => if (a < b) 1 else 0)

  def neq[A: Monoid]: ProgramF[A] = binary[A]("neq", a => b => if (a != b) 1 else 0)

  def mod[A: Monoid]: ProgramF[A] = binary[A]("mod", a => b => b % a)
}
