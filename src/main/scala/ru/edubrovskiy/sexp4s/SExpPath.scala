package ru.edubrovskiy.sexp4s

import cats.implicits._, cats._, cats.derived._

case class SExpPath(indices: List[Int])

object SExpPath {

  implicit val monoidInstance: Monoid[SExpPath] = {
    import cats.derived.auto.monoid._
    semi.monoid
  }

  val empty: SExpPath = monoidInstance.empty
}

