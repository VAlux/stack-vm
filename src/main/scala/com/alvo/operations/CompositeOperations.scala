package com.alvo.operations

import cats.kernel.Monoid
import com.alvo.code.Term
import com.alvo.code.Terms._

object CompositeOperations {

  def range[A: Monoid]: List[Term] = List(
    EXCH,
    SUB,
    REP(List(
      DUP,
      INC)))

  def copy: List[Term] = List(
    EXCH,
    EXCH)

  def memFactIter[A: Monoid]: List[Term] = List(
    DUP,
    PUT(),
    DUP,
    DEC,
    REP(List(
      DEC,
      DUP,
      GET(),
      MUL,
      PUT())),
    GET(),
    SWAP,
    POP)

  def gcd[A: Monoid]: List[Term] = List(
    WHILE(
      copy ++ List(NEQ),
      copy ++ List(LTH, IF(List(NOP), List(SWAP)), EXCH, SUB)),
    POP)
}
