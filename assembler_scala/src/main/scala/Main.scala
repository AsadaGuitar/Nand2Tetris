import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.{catsSyntaxEq, toTraverseOps}

import java.io.FileOutputStream
import scala.io.{BufferedSource, Source}

object Main extends IOApp
  with SymbolTableModule with CodeModule {

  sealed trait Command
  case class CommandC(comp: Option[String], dest: Option[String], jump: Option[String]) extends Command
  case class CommandA(head: String = "0", address: String) extends Command

  def fileReader(path: String): Resource[IO, BufferedSource] =
    Resource.make {
      IO(Source.fromFile(path))
    } { reader =>
      IO(reader.close())
    }

  def fileWriter(path: String): Resource[IO, FileOutputStream] =
    Resource.make {
      IO(new FileOutputStream(path))
    } { writer =>
      IO(writer.close())
    }

  override def run(args: List[String]): IO[ExitCode] = args.headOption match {
    case None => IO.println("Invalid arguments.").as(ExitCode.Success)
    case Some(path) =>
      fileReader(path).use(in => IO(in.getLines().toSeq)).map { assembly =>
        import AssemblyRegex._

        val hack: Option[Seq[String]] = assignAddress(assembly).map {
          case line if aCommandPattern.matches(line) => addressBinary(line.tail)
          case line => commandBinary(line)
        }.sequence

        hack.foreach(lines => lines.foreach(println))

        fileWriter("./output.hack").use { out =>
          hack.fold(IO.println("program error.")){ lines =>
            IO { lines.foreach { line =>
                out.write((line ++ "\n").getBytes)
              }
            }
          }
        }

      }.as(ExitCode.Success)
  }
}

