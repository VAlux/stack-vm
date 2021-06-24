package com.alvo.code

import com.alvo.Program
import com.alvo.Program.id

object TermProgramIsomorphism:
  import cats.Foldable
  import cats.implicits.{given, *}
  import cats.kernel.Monoid
  import com.alvo.Program.idF
  import com.alvo.VirtualMachine.ProgramF
  import com.alvo.code.Terms.Term
  import com.alvo.code.Terms.Term.*
  import com.alvo.operations.ArithmeticOperations.*
  import com.alvo.operations.StackOperations.*

  private def buildHomomorphism[A: Monoid](code: List[Term])(using List[Term] => Program[A]): ProgramF[A] = Foldable[List].foldMap(code) {
    case IF(predicate, body) => branch(predicate, body)(buildHomomorphism(predicate))(buildHomomorphism(body))
    case REP(body) => rep(body)(buildHomomorphism(body))
    case WHILE(predicate, body) => loop(predicate, body)(buildHomomorphism(predicate))(buildHomomorphism(body))
    case PUT(value) => put(value)
    case GET(index) => get(index)
    case PUSH(value) => push(value)
    case NOP => idF
    case POP => pop
    case DUP => dup
    case SWAP => swap
    case EXCH => exch
    case INC => inc
    case DEC => dec
    case ADD => add
    case MUL => mul
    case SUB => sub
    case DIV => div
    case EQL => eqv
    case LTH => lte
    case GTH => gte
    case NEQ => neq
    case NEG => neg
    case MOD => mod
  }

  def fromCode[A: Monoid](code: List[Term])(using List[Term] => Program[A]): ProgramF[A] =
    buildHomomorphism[A](code)

  def toCode[A: Monoid](program: ProgramF[A]): List[Term] = program(id).getProgram.termList

