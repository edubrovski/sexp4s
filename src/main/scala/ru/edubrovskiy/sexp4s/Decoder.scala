package ru.edubrovskiy.sexp4s

import cats.Monad
import ru.edubrovskiy.sexp4s.Decoder.DecodingResult

import scala.annotation.tailrec

trait Decoder[A] {

  def decode(sExp: SExp): DecodingResult[A]
}

object Decoder {

  type DecodingResult[A] = Either[DecodingFailure, A]

  def apply[A](implicit ev: Decoder[A]): Decoder[A] = ev

  def make[A](f: SExp => DecodingResult[A]): Decoder[A] = (sExp: SExp) => f(sExp)

  def failed[A](error: DecodingFailure): DecodingResult[A] = Left(error)

  implicit object MonadInstance extends Monad[Decoder] {

    override def flatMap[A, B](fa: Decoder[A])(f: A => Decoder[B]): Decoder[B] = Decoder.make { sExp =>
      for {
        a <- fa.decode(sExp)
        bDecoder = f(a)
        b <- bDecoder.decode(sExp)
      } yield b
    }

    override def tailRecM[A, B](a: A)(f: A => Decoder[Either[A, B]]): Decoder[B] = Decoder.make { sExp =>

      @tailrec
      def loop(a: A): DecodingResult[B] = f(a).decode(sExp) match {
        case Left(e) => Left(e)
        case Right(Left(a0)) => loop(a0)
        case Right(Right(b)) => Right(b)
      }

      loop(a)
    }

    override def pure[A](x: A): Decoder[A] = Decoder.make(_ => Right(x))
  }
}

