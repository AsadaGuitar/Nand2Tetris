import cats._
import cats.data._
import cats.implicits._
import cats.syntax.all._
import cats.effect._
import cats.kernel.instances.all. _
import scala.language.postfixOps


extension [A](a: Option[A])
  def strict(using Monoid[A], Eq[A]): Option[A] =
    a.flatMap{
        case b if Monoid[A].isEmpty(b) => None
        case b => Some(b)
    }

object Main extends IOApp, ParserModule:
  def run(args: List[String]): IO[ExitCode] =
    val r0: Iterable[Option[String]] = 
      moldAssembly(List("hello", "//hello", "hello//world", "helloworld//", ""))
    val r1 = r0.map(_.strict).flatten
    r1.foreach(println)
    IO.println("HELLO WORLD").as(ExitCode.Success)
