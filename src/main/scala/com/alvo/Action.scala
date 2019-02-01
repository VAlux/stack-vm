package com.alvo

import cats.kernel.Monoid

trait Action[A] {
  val run: A => A
}

object Action {
  // TODO: Not used for now. Consider remove it later...
  implicit def actionCompositionInstance[A]: Monoid[Action[A]] = new Monoid[Action[A]] {
    override def empty: Action[A] = new Action[A] {
      override val run: A => A = identity
    }

    override def combine(f: Action[A], g: Action[A]): Action[A] = new Action[A] {
      override val run: A => A = g.run compose f.run
    }
  }
}