package lib.syntax

import cats.*
import cats.data.*
import cats.implicits.*
import lib.BinaryConvertor


object StringSyntax:

  extension (a: String)
    def binary(using instance: BinaryConvertor[String]): Array[Boolean] = instance.binary(a)
  
  given BinaryConvertor[String] with
    def binary(a: String): Array[Boolean] = binaryOption(a).get
    def binaryOption(a: String): Option[Array[Boolean]] =
      a.map { (char: Char)  =>
        if char === '1' then Some(true) 
        else if char === '0' then Some(false) 
        else None
      }.toList.sequence.map(_.toArray)

  given Conversion[String, Array[Boolean]] with
      def apply(string: String): Array[Boolean] = string.binary
