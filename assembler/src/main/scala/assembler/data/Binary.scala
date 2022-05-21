package assembler.data

import cats._
import cats.data._
import cats.implicits._

object Binary:
    private val binaryPattern = "^[01]+$".r
    private def validate(string: String): Boolean = binaryPattern.matches(string)

    def binaryOption(string: String): Option[Binary] =
        if validate(string) then Some(Binary(string))
        else None

    given Semigroup[Binary] with
        def combine(x: Binary, y: Binary): Binary = 
            Binary(x.toString + y.toString)
    
    
class Binary private[data] (binaryString: String):
    import Binary._
    require(validate(binaryString))
    override def toString = binaryString
