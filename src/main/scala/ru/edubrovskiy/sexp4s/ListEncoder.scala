package ru.edubrovskiy.sexp4s

trait ListEncoder[A] extends Encoder[A] {
  def encode(a: A): SExpList
}

object ListEncoder {

  def apply[A](implicit ev: ListEncoder[A]): ListEncoder[A] = ev

  def make[A](f: A => SExpList): ListEncoder[A] = (a: A) => f(a)
}

