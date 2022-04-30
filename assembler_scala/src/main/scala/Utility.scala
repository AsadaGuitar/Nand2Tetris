import scala.annotation.tailrec

trait Utility {
  implicit class RichString(string: String) {
    def inner: String = {
      @tailrec
      def loop(temp: String, count: Int): String = {
        if (count + 1 < string.length) loop(temp + string(count), count + 1)
        else temp
      }

      loop("", 1)
    }

  }
}
