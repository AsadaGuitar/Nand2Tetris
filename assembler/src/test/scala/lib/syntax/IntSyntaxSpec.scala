package lib.syntax

import cats._
import cats.data._
import cats.implicits._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import lib._
import lib.syntax.IntSyntax.{_, given}


class IntSyntaxSpec extends AnyFlatSpec with Diagrams  with PrivateMethodTester:

    /**
     *  TestType:
     *    All
     *  Function: 
     *    BinaryContertor[Int]#binary(a: A): Seq[Boolean]
     *  Description:
     *    Can convert from numeric to binary.
     */
    "BinaryContertor[Int]" should "can convert Int to Seq[Boolean]." in {
        assert(BinaryConvertor[Int].binary(0) === Seq(false))
        assert(BinaryConvertor[Int].binary(1) === Seq(true))
        assert(BinaryConvertor[Int].binary(2) === Seq(true, false))
        assert(BinaryConvertor[Int].binary(8) === Seq(true,false,false,false))
    }
