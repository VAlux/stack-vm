package com.alvo.loggers

import com.alvo.VirtualMachine
import com.alvo.code.Terms.Term

class CodeLogger extends Logger[List[Term]] {
  override def logOperation: List[Term] => VirtualMachine[List[Term]] => List[Term] = terms => _ => terms

  override def empty: List[Term] = List.empty

  override def combine(x: List[Term], y: List[Term]): List[Term] = x ::: y
}
