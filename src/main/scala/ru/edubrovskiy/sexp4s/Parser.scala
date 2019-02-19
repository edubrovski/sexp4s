package ru.edubrovskiy.sexp4s

import fastparse.Parsed

private[sexp4s] object Parser {

  def parse(input: String): Parsed[SExp] = fastparse.parse(input, sexp(_))

  import fastparse._, NoWhitespace._

  def ws[_: P] = P((" " | "\t" | "\n" | "\r").rep(0))

  def escapedChar[_: P]: P[String] = P("\\" ~ AnyChar.!)

  def normalChar[_: P]: P[String] = P(CharPred(c => c != '\\' && c != '"').!)

  def str[_: P]: P[SExpString] = P("\"" ~ (normalChar | escapedChar).rep.map(chars => SExpString(chars.mkString)) ~ "\"")

  def list[_: P]: P[SExpList] = P("(" ~ ws ~ sexp.rep(sep = ws).map(elems => SExpList(elems.toVector)) ~ ws ~ ")")

  def sexp[_: P]: P[SExp] = P(ws ~ (list | str) ~ ws)
}

