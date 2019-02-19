package ru.edubrovskiy.sexp4s

import org.apache.commons.text.StringEscapeUtils
import shapeless._
import shapeless.labelled.FieldType

import scala.language.higherKinds

object EncoderInstances {

  implicit val intEncoder: Encoder[Int] = Encoder.fromToString

  implicit val doubleEncoder: Encoder[Double] = Encoder.fromToString

  implicit val booleanEncoder: Encoder[Boolean] = Encoder.fromToString

  implicit val stringEncoder: Encoder[String] = Encoder.make(s => SExpString(StringEscapeUtils.escapeJava(s)))

  implicit def collectionEncoder[A : Encoder, C[X] <: TraversableOnce[X]]: Encoder.Aux[C[A], SExpList] = Encoder.make { elems =>
    val encodedElems = elems.toVector.map(encode(_))
    SExpList(encodedElems)
  }

  implicit val hnilEncoder: Encoder.Aux[HNil, SExpList] = Encoder.make(_ => SExpList())

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[Encoder[H]],
    tEncoder: Encoder.Aux[T, SExpList]
  ): Encoder.Aux[FieldType[K, H] :: T, SExpList] = {
    val fieldName = witness.value.name
    Encoder.make { hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val SExpList(tail) = tEncoder.encode(hlist.tail)
      SExpList(SExpList(SExpString(fieldName), head) +: tail)
    }
  }

  implicit def genericObjectEncoder[A, Repr](implicit
    generic: LabelledGeneric.Aux[A, Repr],
    reprEncoder: Lazy[Encoder.Aux[Repr, SExpList]]
  ): Encoder.Aux[A, SExpList] = Encoder.make { a =>
    reprEncoder.value.encode(generic.to(a))
  }
}

