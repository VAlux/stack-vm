package com.alvo

import cats.kernel.Monoid

trait Action[A]:
  def run: A => A

object Action:
  given [A: Monoid]: Monoid[Action[A]] with
    def empty = new Action[A]:
      override val run: A => A = identity

    def combine(f: Action[A], g: Action[A]) = new Action[A]:
      override val run: A => A = g.run compose f.run
