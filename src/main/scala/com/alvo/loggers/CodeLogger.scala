package com.alvo.loggers

import com.alvo.VirtualMachine
import com.alvo.code.Terms.TermList

class CodeLogger extends Logger[TermList] {
  override def logOperation: TermList => VirtualMachine[TermList] => TermList = terms => _ => terms

  override def empty: TermList = List.empty

  override def combine(x: TermList, y: TermList): TermList = x ::: y
}
