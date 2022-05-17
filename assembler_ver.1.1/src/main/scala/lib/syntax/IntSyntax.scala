package lib.syntax

import cats.*
import cats.data.*
import cats.implicits.*

object IntSyntax:
  extension (n: Int) 
    def toBooleanOption: Option[Boolean] =
      if n === 1 then Some(true)
      else if n === 0 then Some(false)
      else None
    def binary(using instance: BinaryConverter[Int]): Binary = instance.binary(n)

  @tailrec
  def binary(n: Int, bin: List[Int] = List.empty[Int]): List[Int] =
    if n / 2 == 1 then 1 :: (n % 2) :: bin
    else
      val q = n / 2; val r = n % 2
      binary(q, r :: bin)
  binary(number).toArray

  given BinaryConvertor[Int]:
    def binary(number: Int): Binary =
      @tailrec
      def loop(n: Int, bin: List[Int] = List.empty[Int]): List[Int] =
        if n / 2 == 1 then 1 :: (n % 2) :: bin
        else
          val q = n / 2; val r = n % 2
          loop(q, r :: bin)
      loop(number).toArray
    def binaryOption(number: int): Option[Binary] = Some(binary(number))