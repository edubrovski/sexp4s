package ru.edubrovskiy.sexp4s

import cats.Contravariant

trait Encoder[A] {

  type Repr <: SExp

  def encode(a: A): Repr
}

object Encoder {

  type Aux[A, Repr0] = Encoder[A] { type Repr = Repr0 }

  def apply[A](implicit ev: Encoder[A]): Encoder[A] = ev

  def make[A, Repr0 <: SExp](f: A => Repr0): Encoder.Aux[A, Repr0] = new Encoder[A] {
    override type Repr = Repr0

    override def encode(a: A): Repr0 = f(a)
  }

  def fromToString[A]: Encoder[A] = make(a => SExpString(a.toString))

  implicit object ContravariantInstance extends Contravariant[Encoder] {

    override def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = Encoder.make { b =>
      val a = f(b)
      fa.encode(a)
    }
  }
}

