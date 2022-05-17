package lib.syntax

import cats.*
import cats.data.*
import cats.implicits.*

import assembler.data.Binary._


object StringSyntax:

  extension (a: String)
    def binary(using instance: BinaryConvertor[String]): Binary = instance.binary(a)
  
  given BinaryConvertor[String] with
    def binary(a: String): Binary = binaryOption(a).get
    def binaryOption(a: String): Option[Binary] =
      a.map { (char: Char)  =>
        if char === '1' then Some(true) 
        else if char === '0' then Some(false) 
        else None
      }.toList.sequence.map(_.toArray)

  given Conversion[String, Binary] with
      def apply(string: String): Binary = string.binary
