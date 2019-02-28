package ru.edubrovskiy.sexp4s

import org.apache.commons.text.StringEscapeUtils
import shapeless._
import shapeless.labelled.FieldType

import scala.language.higherKinds

object EncoderInstances {

  implicit val intEncoder: Encoder[Int] = Encoder.fromToString

  implicit val doubleEncoder: Encoder[Double] = Encoder.fromToString

  implicit val booleanEncoder: Encoder[Boolean] = Encoder.fromToString

  implicit val stringEncoder: Encoder[String] = s => SExpString(StringEscapeUtils.escapeJava(s))

  implicit def collectionEncoder[A : Encoder, C[X] <: TraversableOnce[X]]: ListEncoder[C[A]] = { elems =>
    val encodedElems = elems.toVector.map(encode(_))
    SExpList(encodedElems)
  }

  implicit val hnilEncoder: ListEncoder[HNil] = _ => SExpList()

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[Encoder[H]],
    tEncoder: ListEncoder[T]
  ): ListEncoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    ListEncoder.make { hlist =>
      val headValue = hEncoder.value.encode(hlist.head)
      val SExpList(tail) = tEncoder.encode(hlist.tail)
      val head = SExpList(SExpString(fieldName), headValue)
      SExpList(head +: tail)
    }
  }

  implicit def genericObjectEncoder[A, Repr](implicit
    generic: LabelledGeneric.Aux[A, Repr],
    reprEncoder: Lazy[ListEncoder[Repr]]
  ): ListEncoder[A] = ListEncoder.make { a =>
    reprEncoder.value.encode(generic.to(a))
  }

  implicit val cnilEncoder: ListEncoder[CNil] = _.impossible

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[Encoder[H]],
    tEncoder: ListEncoder[T]
  ): ListEncoder[FieldType[K, H] :+: T] = {
    val fieldName = witness.value.name
    ListEncoder.make {
      case Inl(head) =>
        val fieldValue = hEncoder.value.encode(head)
        SExpList(SExpString(fieldName), fieldValue)
      case Inr(tail) =>
        tEncoder.encode(tail)
    }
  }
}

