package example

import cats.implicits._
import shapeless.LabelledGeneric
import shapeless.labelled.{FieldType, KeyTag}

import scala.collection.generic.CanBuildFrom

object Hello extends App {

  import ru.edubrovskiy.sexp4s._
  import EncoderInstances._
  import DecoderInstances._

  sealed trait Tree[+A]

  case object Leaf extends Tree[Nothing]

  case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

  sealed trait Address

  case class Address1(street: String) extends Address

  case class Address2(city: String, street: String) extends Address

  case class C()

  case class Person(firstName: String, lastName: String, c: C, address: Address)

  val input =
    """(
      |( "firstName" "Eduard")
      |( "lastName" "Dubrovskiy")
      |("c" ())
      |("address" ("Address2" (
      | ("city" "Moscow")
      | ("street" "Milashenkova")
      |)))
      |)
    """.stripMargin

//    """
//      |(
//      | ("firstName" "Eduard")
//      | ("lastName" "Dubrovskiy")
//      |)
//    """.stripMargin

  val sexp = parse(input).get.value

  val decoded = decode[Person](sexp)

  println(decoded)

}

