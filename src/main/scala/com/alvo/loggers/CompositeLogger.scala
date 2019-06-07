package com.alvo.loggers

import com.alvo.VirtualMachine
import com.alvo.code.Terms.TermList

class CompositeLogger[A: Logger, B: Logger] extends Logger[(A, B)] {

  private[this] val logA: Logger[A] = implicitly[Logger[A]]
  private[this] val logB: Logger[B] = implicitly[Logger[B]]

  override def empty: (A, B) = (logA.empty, logB.empty)

  override def combine(a: (A, B), b: (A, B)): (A, B) = (a, b) match {
    case ((a1, b1), (a2, b2)) => (logA.combine(a1, a2), logB.combine(b1, b2))
  }

  override def logOperation: TermList => VirtualMachine[(A, B)] => (A, B) = terms => vm => {
    val vmA = vm.copy(journal = vm.journal._1)
    val vmB = vm.copy(journal = vm.journal._2)

    (logA.logOperation.apply(terms)(vmA), logB.logOperation.apply(terms)(vmB))
  }
}

object CompositeLogger {
  implicit def compositeLoggerInstanceProvider[A: Logger, B: Logger]: CompositeLogger[A, B] = new CompositeLogger[A, B]
}
