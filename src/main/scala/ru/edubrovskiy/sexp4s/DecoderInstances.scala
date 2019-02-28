package ru.edubrovskiy.sexp4s

import cats.implicits._
import ru.edubrovskiy.sexp4s.Decoder.DecodingResult
import shapeless._
import shapeless.labelled._
import monocle.macros.syntax.lens._
import scala.language.higherKinds
import scala.util.Try

object DecoderInstances {

  def makeError(error: String): DecodingFailure =
    DecodingFailure(SExpPath.empty, error)

  def makeCollectionDecoder[A : Decoder, C[_]](toCollection: Vector[A] => C[A]): Decoder[C[A]] = Decoder.make {
    case SExpList(values) => values.zipWithIndex.traverse[DecodingResult, A] { case (elem, index) =>
      decode[A](elem).leftMap { error =>
        error.lens(_.path.indices).modify(index :: _)
      }
    }.map(toCollection)
    case exp => Left(makeError(s"expected list, got $exp"))
  }

  type FieldMap = Map[String, (Int, SExp)]

  trait FromMapDecoder[A] {
    def decode(fields: FieldMap, originalObject: SExp): DecodingResult[A]
  }

  object FromMapDecoder {

    def make[A](f: (FieldMap, SExp) => DecodingResult[A]): FromMapDecoder[A] =
      (fields: FieldMap, obj: SExp) => f(fields, obj)
  }

  implicit val hnilFromMapDecoder: FromMapDecoder[HNil] = FromMapDecoder.make((_, _) => Right(HNil))

  def toFieldMapOrError(obj: SExp): DecodingResult[FieldMap] = {

    def fromListOfFields(fields: Vector[SExp]) =
      fields.zipWithIndex.traverse[DecodingResult, (String, (Int, SExp))] { case (field, index) =>
        field match {
          case SExpList(Vector(SExpString(fieldName), fieldValue)) => Right(fieldName -> (index, fieldValue))
          case _ => Left(makeError(s"expected list (fieldName fieldValue), got ${field.prettyPrint()}"))
        }
      }

    obj match {
      case SExpList(fields) => fromListOfFields(fields).map(_.toMap)
      case _ => Left(makeError(s"expected list of fields, got ${obj.prettyPrint()}"))
    }
  }

  implicit def hlistFromMapDecoder[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    hDecoder: Lazy[Decoder[H]],
    tDecoder: FromMapDecoder[T]
  ): FromMapDecoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    FromMapDecoder.make { (fieldMap, obj) =>
      fieldMap.get(fieldName) match {
        case None => Left(makeError(s"no '$fieldName' field on s-exp ${obj.prettyPrint()}"))
        case Some((index, value)) =>
          val result = for {
            h <- hDecoder.value.decode(value)
            t <- tDecoder.decode(fieldMap, obj)
          } yield field[K](h) :: t

          result.leftMap { error =>
            error.lens(_.path.indices).modify(index :: _)
          }
      }
    }
  }

  implicit val intDecoder: Decoder[Int] = Decoder.make {
    case s: SExpString => Try(s.value.toInt).toEither.leftMap(_ => makeError(s"expected int, got $s"))
    case exp => Left(makeError(s"expected int, got $exp"))
  }

  implicit val stringDecoder: Decoder[String] = Decoder.make {
    case SExpString(s) => Right(s)
    case sExp => Left(makeError(s"expected string, got $sExp"))
  }

  implicit def listDecoder[A : Decoder]: Decoder[List[A]] = makeCollectionDecoder(_.toList)

  implicit def vectorDecoder[A : Decoder]: Decoder[Vector[A]] = makeCollectionDecoder(identity)

  implicit def hlistDecoder[A, Repr](implicit
    gen: LabelledGeneric.Aux[A, Repr],
    decoder: Lazy[FromMapDecoder[Repr]]
  ): Decoder[A] = Decoder.make { obj =>
    for {
      fieldMap <- toFieldMapOrError(obj)
      repr <- decoder.value.decode(fieldMap, obj)
    } yield gen.from(repr)
  }

  trait FromPairDecoder[A] {
    def decode(name: String, value: SExp): DecodingResult[A]
  }

  object FromPairDecoder {
    def make[A](f: (String, SExp) => DecodingResult[A]): FromPairDecoder[A] =
      (name: String, sExp: SExp) => f(name, sExp)
  }

  implicit val cnilFromPairDecoder: FromPairDecoder[CNil] = FromPairDecoder.make { (name, _) =>
    Left(makeError(s"unknown coproduct component '$name'"))
  }

  implicit def coproductFromPairDecoder[K <: Symbol, H, T <: Coproduct](implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[Decoder[H]],
    tEncoder: FromPairDecoder[T]
  ): FromPairDecoder[FieldType[K, H] :+: T] = {
    val componentName = witness.value.name
    FromPairDecoder.make { (name, value) =>
      if (name == componentName)
        hEncoder.value.decode(value).map(h => Inl(field[K](h)))
      else
        tEncoder.decode(name, value).map(Inr(_))
    }
  }

  def toPairOrError(sExp: SExp): DecodingResult[(String, SExp)] = sExp match {
    case SExpList(Vector(SExpString(name), value)) => Right(name -> value)
    case _ => Left(makeError(s"expected (name value) list, got ${sExp.prettyPrint()}"))
  }

  implicit def coproductDecoder[A, Repr](implicit
    gen: LabelledGeneric.Aux[A, Repr],
    decoder: Lazy[FromPairDecoder[Repr]]
  ): Decoder[A] = Decoder.make { sExp =>
    for {
      pair <- toPairOrError(sExp)
      (name, value) = pair
      repr <- decoder.value.decode(name, value)
    } yield gen.from(repr)
  }
}

