import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName, SelfAwareStructuredLogger}
import org.typelevel.log4cats.slf4j.{Slf4jFactory, Slf4jLogger}
import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import cats.implicits._

import java.io.FileInputStream


object Main extends IOApp {

  implicit val logging: LoggerFactory[IO] = Slf4jFactory[IO]
  def logger: SelfAwareStructuredLogger[IO] = Slf4jFactory[IO].getLogger

  def fileReader(file: String): Resource[IO, FileInputStream] =
    Resource.make(IO(new FileInputStream(file)))(file => IO(file.close()))

  override def run(args: List[String]): IO[ExitCode] = {
    val app = args.headOption match {
      case None => IO.println("No argument exists.")
      case Some(input) if input.split("\\.").lastOption.fold(false)(_=="vm") =>
        for {
          _ <- logger.info("start main.")
          _ <- logger.info(s"input file is $input")
          file <- fileReader(input).use(_ => IO(""))
          _ <- logger.info("end main.")
        } yield ()
    }

    app.as(ExitCode.Success)
  }
}
