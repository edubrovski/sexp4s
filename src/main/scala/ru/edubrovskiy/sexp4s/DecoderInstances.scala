package ru.edubrovskiy.sexp4s

import cats.implicits._
import ru.edubrovskiy.sexp4s.Decoder.DecodingResult
import shapeless._
import shapeless.labelled._
import monocle.macros.syntax.lens._
import scala.language.higherKinds
import scala.util.Try

object DecoderInstances {

  private[this] def makeError(error: String): DecodingFailure =
    DecodingFailure(SExpPath.empty, error)

  private[this] def makeCollectionDecoder[A : Decoder, C[_]](toCollection: Vector[A] => C[A]): Decoder[C[A]] = Decoder.make {
    case SExpList(values) => values.zipWithIndex.traverse[DecodingResult, A] { case (elem, index) =>
      decode[A](elem).leftMap { error =>
        error.lens(_.path.indices).modify(index :: _)
      }
    }.map(toCollection)
    case exp => Left(makeError(s"expected list, got $exp"))
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

  implicit val hnilDecoder: Decoder[HNil] = Decoder.make {
    case SExpList(_) => Right(HNil)
    case exp => Left(makeError(s"expected list, got $exp"))
  }

  implicit def hlistObjectDecoder[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    hDecoder: Lazy[Decoder[H]],
    tDecoder: Decoder[T]
  ): Decoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    Decoder.make {
      case sExp @ SExpList(fields) =>
        val maybeField = fields.zipWithIndex.collectFirst {
          case (SExpList(Vector(SExpString(`fieldName`), v)), i) => (v, i)
        }

        val decodeHeadResult = maybeField match {
          case None => Left(makeError(s"'$fieldName' field was not found on $sExp"))
          case Some((exp, index)) => hDecoder.value.decode(exp).leftMap { error =>
            error.lens(_.path.indices).modify(index :: _)
          }
        }

        for {
          head <- decodeHeadResult
          tail <- tDecoder.decode(sExp)
        } yield field[K](head) :: tail
      case exp => Left(makeError(s"expected list, got $exp"))
    }
  }

  implicit def genericObjectDecoder[A, Repr](implicit
    generic: LabelledGeneric.Aux[A, Repr],
    reprDecoder: Lazy[Decoder[Repr]]
  ): Decoder[A] = Decoder.make { sExp =>
    reprDecoder.value.decode(sExp).map(generic.from)
  }
}

