package ru.edubrovskiy.sexp4s

import cats.Show
import cats.implicits._

sealed trait SExp {

  def prettyPrint(tabSize: Int = 2): String = {
    val oneTab = " " * tabSize

    def printExp(exp: SExp, indentation: Int): String = {
      val tab = oneTab * indentation
      exp match {
        case SExpString(value) => tab + value
        case SExpList(values) if values.exists(_.isInstanceOf[SExpList]) =>
          s"$tab(\n" +
            values.map(printExp(_, indentation + 1)).mkString("\n") + "\n" +
            s"$tab)"
        case SExpList(values) => s"$tab(${values.map(_.show).mkString(" ")})"
      }
    }

    printExp(this, 0)
  }

  override def toString: String = prettyPrint()
}

case class SExpString(value: String) extends SExp

case class SExpList(values: Vector[SExp]) extends SExp

object SExp {
  def parse(input: String): fastparse.Parsed[SExp] = Parser.parse(input)

  implicit val showInstance: Show[SExp] = Show.show {
    case s: SExpString => s.show
    case s: SExpList => s.show
  }
}

object SExpString {
  implicit val showInstance: Show[SExpString] = Show.show(s => "\"" + s.value + "\"")
}

object SExpList {

  def apply(values: SExp*): SExpList = new SExpList(values.toVector)

  implicit val showInstance: Show[SExpList] = Show.show { list =>
    "(" + list.values.mkString(" ") + ")"
  }
}

