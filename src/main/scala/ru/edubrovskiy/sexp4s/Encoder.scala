package ru.edubrovskiy.sexp4s

import cats.Contravariant

trait Encoder[A] {
  def encode(a: A): SExp
}

object Encoder {

  def apply[A](implicit ev: Encoder[A]): Encoder[A] = ev

  def make[A](f: A => SExp): Encoder[A] = (a: A) => f(a)

  def fromToString[A]: Encoder[A] = make(a => SExpString(a.toString))

  implicit object ContravariantInstance extends Contravariant[Encoder] {

    override def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = { b: B =>
      val a = f(b)
      fa.encode(a)
    }
  }
}

