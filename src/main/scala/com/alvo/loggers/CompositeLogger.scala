package com.alvo.loggers

import com.alvo.VirtualMachine
import com.alvo.code.Terms.Term

class CompositeLogger[A: Logger, B: Logger] extends Logger[(A, B)]:
  private[this] val logA: Logger[A] = summon[Logger[A]]
  private[this] val logB: Logger[B] = summon[Logger[B]]

  override def empty: (A, B) = (logA.empty, logB.empty)

  override def combine(a: (A, B), b: (A, B)): (A, B) = (a, b) match
    case ((a1, b1), (a2, b2)) => (logA.combine(a1, a2), logB.combine(b1, b2))

  override def logOperation: List[Term] => VirtualMachine[(A, B)] => (A, B) = terms => vm => {
    val vmA = vm.copy(journal = vm.journal._1)
    val vmB = vm.copy(journal = vm.journal._2)

    (logA.logOperation.apply(terms)(vmA), logB.logOperation.apply(terms)(vmB))
  }

object CompositeLogger:
  given [A: Logger, B: Logger]: CompositeLogger[A, B] = new CompositeLogger[A, B]
