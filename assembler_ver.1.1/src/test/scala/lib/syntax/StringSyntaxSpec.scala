package lib.syntax


import cats._
import cats.data._
import cats.implicits._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import lib._
import lib.syntax.StringSyntax.{_, given}


class StringSyntaxSpec extends AnyFlatSpec with Diagrams  with PrivateMethodTester:

  "BinaryConvertor[String]" should "Can convertes String to Array[Boolean]." in {
      assert(BinaryConvertor[String].binary("111") === Seq(true,true,true))
      assert(BinaryConvertor[String].binary("000") === Seq(false, false, false))
      assert(BinaryConvertor[String].binary("101") === Seq(true, false, true))
  }

  "Extension binary" should "is can to call form String." in {
    assert("000".binary === Seq(false, false, false))
    assert("111".binary === Seq(true, true, true))
  }

end StringSyntaxSpec