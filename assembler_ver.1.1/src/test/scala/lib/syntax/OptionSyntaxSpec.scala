package lib.syntax

import cats._
import cats.data._
import cats.implicits._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import lib._
import lib.syntax.OptionSyntax.{_, given}


class OptionSyntaxSpec extends AnyFlatSpec with Diagrams  with PrivateMethodTester:

    "StrictOne[Option]" should "can set the EMPTY element to None." in {
        assert(StrictOne[Option].strict(Some(1)) === Some(1))
        assert(StrictOne[Option].strict(Some("")) === None)
        assert(StrictOne[Option].strict(Some(0)) === None)
        assert{
            val test: Option[Int] = None
            StrictOne[Option].strict(test).isEmpty
        }
    }

    "Extension strict" should "can call from Option[A]." in {
        assert(Some(1).strict === Some(1))
        assert(Some(0).strict === None)
        assert(Some("").strict === None)
        assert {
            val test: Option[Int] = None
            test.strict.isEmpty
        }
    }

end OptionSyntaxSpec