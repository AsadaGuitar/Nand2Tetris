package lib.syntax

import cats.*
import cats.data.*
import cats.implicits.*

import lib.BinaryConvertor
import lib.syntax.IntSyntax.{_, given}


object StringSyntax:

  extension (a: String)
    def binary(using instance: BinaryConvertor[String]): Seq[Boolean] = instance.binary(a)
  
  given BinaryConvertor[String] with
    def binary(string: String): Seq[Boolean] = 
      string.map { char => 
        if char === '0' then false else true
      }
    
end StringSyntax