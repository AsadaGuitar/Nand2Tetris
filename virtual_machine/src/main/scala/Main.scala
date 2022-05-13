import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName, SelfAwareStructuredLogger}
import org.typelevel.log4cats.slf4j.{Slf4jFactory, Slf4jLogger}
import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import cats.implicits._

import scala.util.parsing.combinator._
import java.io.FileInputStream
import java.lang.Error
import scala.util.{Failure, Success}


case class WordFreq(word: String, count: Int) {
  override def toString = s"Word <$word> occurs with frequency $count"
}

class SimpleParser extends RegexParsers {
  def word: Parser[String]   = """[a-z]+""".r       ^^ { _.toString }
  def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def freq: Parser[WordFreq] = word ~ number        ^^ { case wd ~ fr => WordFreq(wd,fr) }

}

class AssemblyParser extends JavaTokenParsers {
  def symbol: Parser[String] = rep("@" ~ stringLiteral) ^^ { _.toString }
  def value: Parser[String] =  this.regex("^\\d{1,4}$".r) | stringLiteral
}

object Main extends SimpleParser {

  implicit val logging: LoggerFactory[IO] = Slf4jFactory[IO]
  def logger: SelfAwareStructuredLogger[IO] = Slf4jFactory[IO].getLogger

  def fileReader(file: String): Resource[IO, FileInputStream] =
    Resource.make(IO(new FileInputStream(file)))(file => IO(file.close()))

  def main(args: Array[String]): Unit = {
    parse(freq, "johnny 121") match {
      case Success(matched,_) => println(matched)
      case Failure(msg,_) => println(s"FAILURE: $msg")
      case Error(msg,_) => println(s"ERROR: $msg")
    }
  }
}
