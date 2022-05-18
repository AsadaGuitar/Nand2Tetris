package lib.syntax

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import lib.syntax.StringSyntax.{_, given}
import lib._


class StringSyntaxSpec extends AnyFlatSpec with Diagrams  with PrivateMethodTester:
  
  "BinaryConvertor[String]" should "Can converte String to Array[Boolean]" in {
      assert(BinaryConvertor[String].binary("000") === Array(false, false, false))
      assert(BinaryConvertor[String].binary("111") === Array(true, true, true))
      
  }