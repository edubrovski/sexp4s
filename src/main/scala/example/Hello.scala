package example

import cats.implicits._
import shapeless.labelled.{FieldType, KeyTag}

import scala.collection.generic.CanBuildFrom

object Hello extends App {

  import ru.edubrovskiy.sexp4s._
  import EncoderInstances._
  import DecoderInstances._

  case class Address(city: String, street: String)

  case class Person(firstName: String, lastName: String, address: Address)

  import shapeless._

  val sexp = parse(
    """
      |(
      |("firstName" "Eduard")
      |("lastName" "Dubrovskiy")
      |)
    """.stripMargin).get.value

  val person = decode[Person](sexp)

  println(person)

//  val p1 = Person("Eduard", "Dubrovskiy")
//  val p2 = Person("Martin", "Odersky")
//
//
//
//  val sexp = encode(List(p1, p2))
//
//  val str = sexp.prettyPrint()
//
//  val parsed = parse(str).get.value
//
//  println(parsed.show)
}

