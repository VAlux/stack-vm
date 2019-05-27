package com.alvo.operations

import cats.kernel.Monoid
import com.alvo.code.Term
import com.alvo.code.Terms.{REP, _}

object CompositeOperations {

  def range[A: Monoid]: List[Term] = EXCH :: SUB :: REP(DUP :: INC :: Nil) :: Nil

  def copy: List[Term] = EXCH :: EXCH :: Nil

  def memFactIterHom[A: Monoid]: List[Term] =
    DUP :: PUT() :: DUP :: DEC ::
      REP(DEC :: DUP :: GET() :: MUL :: PUT() :: Nil) ::
      GET() :: SWAP :: POP :: Nil

  def gcd[A: Monoid]: List[Term] =
    WHILE(
      copy ::: NEQ :: Nil,
      copy ::: LTH :: IF(ID :: Nil, SWAP :: Nil) :: EXCH :: SUB :: Nil
    ) :: POP :: Nil
}
