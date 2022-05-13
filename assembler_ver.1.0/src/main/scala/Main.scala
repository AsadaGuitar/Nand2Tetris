import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.{catsSyntaxEq, toTraverseOps}

import java.io.FileOutputStream
import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

object Main extends IOApp
  with ParserModule with SymbolTableModule with CodeModule {



  private def fileReader(path: String): Resource[IO, BufferedSource] =
    Resource.make {
      IO(Source.fromFile(path))
    } { reader =>
      IO(reader.close())
    }

  private def fileWriter(path: String): Resource[IO, FileOutputStream] =
    Resource.make {
      IO(new FileOutputStream(path))
    } { writer =>
      IO(writer.close())
    }

  private def getFileName(input: String): String ={
    val temp = input.split("\\.").flatMap(s => s.split("/"))
    temp(temp.length -2)
  }

  override def run(args: List[String]): IO[ExitCode] = args.headOption match {
    case None => IO.println("Invalid arguments.").as(ExitCode.Success)
    case Some(path) if path.split("\\.").lastOption.fold(false)(_==="asm") =>
      val outputFileName = getFileName(path) ++ ".hack"
      val hack: IO[Either[Throwable, Option[Seq[String]]]] = fileReader(path).use{ in =>
        val assembler = parseAssembly andThen assignAddress andThen assemblyBinary
        IO(assembler(in.getLines().toSeq).sequence)
      }.attempt
      fileWriter(outputFileName).use{ out =>
        hack.map {
          case Right(value) => value.fold(println("Invalid assembly.")) { asm =>
            asm.foreach(line => out.write((line ++ "\n").getBytes))
            println(s"Converted machine lang from $outputFileName.")
          }
          case Left(exception) => println(exception.getMessage)
        }
      }.as(ExitCode.Success)
    case _ => IO.println("Invalid file extension.").as(ExitCode.Success)
  }
}
