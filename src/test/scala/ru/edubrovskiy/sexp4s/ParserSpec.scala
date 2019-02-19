package ru.edubrovskiy.sexp4s

import fastparse.Parsed
import fastparse.Parsed.Success
import org.specs2

class ParserSpec extends org.specs2.mutable.Specification {

  def parseAndAssertSuccess(input: String): SExp = {
    val result = Parser.parse(input)

    if (!result.isSuccess) {
      println(s"Failed to parse input $input. Result: $result")
    }

    result must haveClass[Parsed.Success[SExp]]

    result.get.value
  }

  "parse number" >> {
    val result = parseAndAssertSuccess(""""123.45"""")

    result must_== SExpString("123.45")
  }

  "parse empty string" >> {
    val result = parseAndAssertSuccess("""""""")

    result must_== SExpString("")
  }

  "parse backslash" >> {
    val result = parseAndAssertSuccess(""""here's a backslash: \\"""")

    result must_== SExpString("here's a backslash: \\")
  }

  "parse double quote" >> {
    val result = parseAndAssertSuccess(""""here's some \"quoted\" text"""")

    result must_== SExpString("here's some \"quoted\" text")
  }

  "parse list" >> {
    val result = parseAndAssertSuccess("""("a" "b" "c")""")

    result must_== SExpList(SExpString("a"), SExpString("b"), SExpString("c"))
  }

  "parse nested list" >> {
    val result = parseAndAssertSuccess("""(("a" "b") "c")""")

    result must_== SExpList(
      SExpList(SExpString("a"), SExpString("b")),
      SExpString("c")
    )
  }

  "parse multiline sexp" >> {
    val result = parseAndAssertSuccess(
      """
        |(
        |  ( "a" "b" )
        |  "c"
        |)
      """.stripMargin)

    result must_== SExpList(
      SExpList(SExpString("a"), SExpString("b")),
      SExpString("c")
    )
  }
}
