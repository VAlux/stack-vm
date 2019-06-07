package com.alvo.operations

import cats.kernel.Monoid
import cats.syntax.show._
import com.alvo.Program
import com.alvo.VirtualMachine.{ProgramF, Stack, error}
import com.alvo.code.Term
import com.alvo.code.ShowTerm._
import com.alvo.code.Terms._

object ArithmeticOperations {

  private[this] implicit val intToStack: Int => Stack = _ :: Nil

  private def unary[A: Monoid](term: Term)(operation: Int => Stack): ProgramF[A] =
    Program.createProgramForStack[A](term) { stack =>
      vm =>
        stack match {
          case x :: xs => vm.setStack(operation(x) ++ xs)
          case _ => error(show"operation $term expected an argument").apply(vm)
        }
    }

  private def binary[A: Monoid](term: Term)(operation: Int => Int => Stack): ProgramF[A] =
    Program.createProgramForStack[A](term) { stack =>
      vm =>
        stack match {
          case x :: y :: xs => vm.setStack(operation(x)(y) ++ xs)
          case _ => error(show"operation $term expected 2 arguments").apply(vm)
        }
    }

  def neg[A: Monoid]: ProgramF[A] = unary[A](ADD)(a => -a)

  def inc[A: Monoid]: ProgramF[A] = unary[A](INC)(a => a + 1)

  def dec[A: Monoid]: ProgramF[A] = unary[A](DEC)(a => a - 1)

  def add[A: Monoid]: ProgramF[A] = binary[A](ADD)(a => b => a + b)

  def sub[A: Monoid]: ProgramF[A] = binary[A](SUB)(a => b => b - a)

  def mul[A: Monoid]: ProgramF[A] = binary[A](MUL)(a => b => a * b)

  def div[A: Monoid]: ProgramF[A] = binary[A](DIV)(a => b => b / a)

  def eqv[A: Monoid]: ProgramF[A] = binary[A](EQL)(a => b => if (a == b) 1 else 0)

  def lte[A: Monoid]: ProgramF[A] = binary[A](LTH)(a => b => if (a > b) 1 else 0)

  def gte[A: Monoid]: ProgramF[A] = binary[A](GTH)(a => b => if (a < b) 1 else 0)

  def neq[A: Monoid]: ProgramF[A] = binary[A](NEQ)(a => b => if (a != b) 1 else 0)

  def mod[A: Monoid]: ProgramF[A] = binary[A](MOD)(a => b => b % a)
}
