import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.{catsSyntaxEq, toTraverseOps}

import java.io.FileOutputStream
import scala.io.{BufferedSource, Source}

object Main extends IOApp
  with ParserModule with SymbolTableModule with CodeModule {
  import AssemblyRegex._

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

  private def getFileName(input: String): String ={
    val temp = input.split("\\.").flatMap(s => s.split("/"))
    temp(temp.length -2)
  }

  override def run(args: List[String]): IO[ExitCode] = args.headOption match {
    case None => IO.println("Invalid arguments.").as(ExitCode.Success)
    case Some(path) if path.split("\\.").lastOption.fold(false)(_==="asm") =>
      val hack: IO[Option[Seq[String]]] = fileReader(path).use{ in =>
        val parsedAsm = this.parseAssembly(in.getLines().toSeq)
        val assignedAsm = this.assignAddress(parsedAsm)
        val hackAssembly = assignedAsm.map {
          case line@aCommandPattern() => addressBinary(line)
          case line@mnemonicPattern() => commandBinary(line)
          case line => println(s"match error: $line"); None
        }
        IO(hackAssembly.sequence)
      }
      val outputFileName = getFileName(path) ++ ".hack"
      fileWriter(outputFileName).use{ out =>
        hack.map { asm =>
          asm.fold(println("Invalid assembly.")){ asm =>
            asm.foreach{ line =>
              out.write((line + "\n").getBytes)
            }
            println("Converted to machine language.")
          }
        }
      }.as(ExitCode.Success)
    case _ => IO.println("Invalid file extension.").as(ExitCode.Success)
  }
}

