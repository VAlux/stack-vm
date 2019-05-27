package com.alvo.code

sealed trait Term

object Terms {

  type TermSeq = List[Term]

  case class IF(predicate: TermSeq, body: TermSeq) extends Term

  case class REP(body: TermSeq) extends Term

  case class WHILE(predicate: TermSeq, body: TermSeq) extends Term

  case class PUT(value: Int = 0) extends Term

  case class GET(index: Int = 0) extends Term

  case class PUSH(value: Int = 0) extends Term

  case object ID extends Term

  case object POP extends Term

  case object DUP extends Term

  case object SWAP extends Term

  case object EXCH extends Term

  case object INC extends Term

  case object DEC extends Term

  case object NEG extends Term

  case object ADD extends Term

  case object MUL extends Term

  case object SUB extends Term

  case object DIV extends Term

  case object EQL extends Term

  case object LTH extends Term

  case object GTH extends Term

  case object NEQ extends Term

}

object Hom {

  import cats.implicits._
  import cats.Foldable
  import cats.kernel.Monoid
  import com.alvo.Program.idF
  import com.alvo.VirtualMachine.ProgramF
  import com.alvo.operations.ArithmeticOperations._
  import com.alvo.operations.StackOperations._
  import com.alvo.code.Terms._

  def fromCode[A: Monoid](code: TermSeq): ProgramF[A] = {

    def buildHomomorphism(code: TermSeq): ProgramF[A] = Foldable[List].foldMap(code) {
      case IF(predicate, body) => branch(buildHomomorphism(predicate))(buildHomomorphism(body))
      case REP(body) => rep(buildHomomorphism(body))
      case WHILE(predicate, body) => loop(buildHomomorphism(predicate))(buildHomomorphism(body))
      case PUT(value) => put(value)
      case GET(index) => get(index)
      case PUSH(value) => push(value)
      case ID => idF
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
    }

    buildHomomorphism(code)
  }
}


