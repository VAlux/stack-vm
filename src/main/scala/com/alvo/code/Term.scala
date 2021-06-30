package com.alvo.code

import cats.Show

enum Term:
  case IF(predicate: List[Term], body: List[Term])
  case REP(body: List[Term])
  case WHILE(predicate: List[Term], body: List[Term])
  case PUT(value: Int = 0)
  case GET(index: Int = 0)
  case PUSH(value: Int = 0)
  case NOP
  case POP
  case DUP
  case SWAP
  case EXCH
  case INC
  case DEC
  case NEG
  case ADD
  case MUL
  case SUB
  case DIV
  case EQL
  case LTH
  case GTH
  case NEQ
  case MOD

object Term:
  given Conversion[Term, List[Term]] = List(_)
  given Show[Term] = _.getClass.getSimpleName
