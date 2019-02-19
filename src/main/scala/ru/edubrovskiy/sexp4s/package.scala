package ru.edubrovskiy

import fastparse.Parsed
import ru.edubrovskiy.sexp4s.Decoder.DecodingResult

package object sexp4s {

  def parse(input: String): Parsed[SExp] = Parser.parse(input)

  def decode[A](sExp: SExp)(implicit decoder: Decoder[A]): DecodingResult[A] = decoder.decode(sExp)

  def encode[A](a: A)(implicit encoder: Encoder[A]): SExp = encoder.encode(a)
}

