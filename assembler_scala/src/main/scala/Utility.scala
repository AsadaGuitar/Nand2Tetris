import scala.annotation.tailrec

object Utility {
  
  def inner(string: String): String ={
    @tailrec
    def loop(temp: String, count: Int): String = {
      if (count + 1 < string.length) loop(temp + string(count), count + 1)
      else temp
    }
    loop("", 1)
  }

  def bin16(number: Int): Either[IndexOutOfBoundsException, Array[Int]] = {
    val bin = new Array[Int](16)
    @tailrec
    def loop(n: Int, idx: Int = 15): Either[IndexOutOfBoundsException, Array[Int]] = {
      if (n < 2) {
        bin(idx) = n % 2
        Right(bin)
      } else {
        bin(idx) = n % 2
        if (idx == 0) {
          Left(new IndexOutOfBoundsException("IndexOutOfBoundsException from 'bin16'."))
        }
        loop(n / 2, idx -1)
      }
    }
    loop(number)
  }
}
