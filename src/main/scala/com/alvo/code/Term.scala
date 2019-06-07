package com.alvo.code

import cats.Show

sealed trait Term

object Terms {

  type TermList = List[Term]

  implicit val termToTermSeq: Term => List[Term] = t => List(t)

  case class IF(predicate: TermList, body: TermList) extends Term

  case class REP(body: TermList) extends Term

  case class WHILE(predicate: TermList, body: TermList) extends Term

  case class PUT(value: Int = 0) extends Term

  case class GET(index: Int = 0) extends Term

  case class PUSH(value: Int = 0) extends Term

  case object NOP extends Term

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

  case object MOD extends Term

}

object ShowTerm {
  implicit val showTerm: Show[Term] = (term: Term) => term.getClass.getSimpleName
}
