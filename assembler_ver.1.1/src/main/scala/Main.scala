import cats._
import cats.data._
import cats.implicits._
import cats.syntax.all._
import cats.effect._
import cats.kernel.instances.all. _
import scala.language.postfixOps



trait Strict[F[_]]:
  extension [A](a: F[A]) def strict(using Monad[F], Monoid[A], Eq[A]): F[A]

given Strict[Option] with
   extension [A](a: Option[A]) def strict(using Monad[Option], Monoid[A], Eq[A]): Option[A] =
    a.flatMap{
        case b if Monoid[A].isEmpty(b) => None
        case b => Some(b)
    }

extension [A, F[_]](a: Traversable[F[A]])(using Monad[F], Strict[F])
  def strictAll(using Monoid[A], Eq[A]): Traversable[F[A]] = a.map(_.strict)

object Main extends IOApp, ParserModule:
  def run(args: List[String]): IO[ExitCode] =
    val r0: Iterable[Option[String]] = 
      moldAssembly(List("hello", "//hello", "hello//world", "helloworld//", ""))
    val r1 = r0.strictAll
    r1.foreach(println)
    IO.println("HELLO WORLD").as(ExitCode.Success)
