import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName, SelfAwareStructuredLogger}
import org.typelevel.log4cats.slf4j.{Slf4jFactory, Slf4jLogger}
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._


object Main extends IOApp {

  implicit val logging: LoggerFactory[IO] = Slf4jFactory[IO]
  def logger[F[_]: Sync]: SelfAwareStructuredLogger[F] = Slf4jFactory[F].getLogger

  def doSomething[F[_]: Sync](): F[Unit] =
    logger[F].info("Logging Start Something") *>
      Sync[F].delay(println("I could be doing anything")).attempt.flatMap {
        case Left(e)  => logger[F].error(e)("Something Went Wrong")
        case Right(_) => logger[F].info("Something is success.")
      }

  override def run(args: List[String]): IO[ExitCode] = {
    val startLog = logger[IO].info("start main.")
    val endLog = logger[IO].info("end main.")

    startLog *> doSomething[IO]() *> endLog.as(ExitCode.Success)
  }
}
