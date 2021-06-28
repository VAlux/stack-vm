package com.alvo

import cats.kernel.Monoid

trait Action[A]:
  def run: A => A

object Action:
  given [A: Monoid]: Monoid[Action[A]] = new Monoid[Action[A]]:
    override def empty = new Action[A]:
      override val run: A => A = identity

    override def combine(f: Action[A], g: Action[A]) = new Action[A]:
      override val run: A => A = g.run compose f.run