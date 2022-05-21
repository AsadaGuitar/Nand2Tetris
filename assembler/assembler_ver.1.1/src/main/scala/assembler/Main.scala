package assembler

import cats._
import cats.data._
import cats.implicits._
import cats.effect.*
import cats.kernel.instances.all.*

import scala.collection.JavaConverters._

import java.io._

import assembler.module.ParserModule
import assembler.module.SymbolTableModule

import assembler.data.AssemblyLine.{_, given}
import assembler.data.Symbol.{_, given}
import assembler.data.Mnemonic.{_, given}

import lib._
import lib.syntax.IntSyntax.{_, given}


/*
benchmark
ver.1 => 150ms
ver.2 => 151ms

*/

object Main extends IOApp, ParserModule, SymbolTableModule:
  import cats.data.Validated._

  def reader(path: String) = Resource.fromAutoCloseable(IO(new BufferedReader(new FileReader(path))))
  def writer(path: String) = Resource.fromAutoCloseable(IO(new BufferedWriter(new FileWriter(path))))
    
  def validateAssembly(assembly: Vector[String]): ValidatedNec[String, List[PassedInstruction]] = 
    moldAssembly(assembly).zipWithIndex.map { (line, i) =>
        parseAll(assemblyParser, line) match 
        case Success(value, _) => value.validNec
        case NoSuccess(msg, _) => s"syntax error, line ${i+1}: $msg.".invalidNec
        case Failure(msg, _)   => s"failure, line ${i+1}: $msg.".invalidNec
        case Error(msg, _)     => s"error, line ${i+1}: $msg.".invalidNec
    }.toList.sequence

  def inputPathParser(string: String): Option[String] =
    val temp = string.split("\\.")
    if 2 <= temp.length && temp(temp.length-1) === "asm" then
      Some(temp.take(temp.length-1).mkString("."))
    else None

  def run(args: List[String]): IO[ExitCode] =
    val start = System.currentTimeMillis()
    args.headOption match
      case Some(inputPath) =>
        inputPathParser(inputPath) match 
          case Some(inputPathName) => 
            val outputPath = inputPathName + ".hack"
            val readAssembly = reader(inputPath).use { bf =>
              IO{ bf.lines.iterator().asScala.toVector }
            }
            readAssembly.flatMap{ assembly => 
              validateAssembly(assembly).map(assignAddress) match
                case Invalid(messages) => IO { messages.map(println) }
                case Valid(assigns)    => writer(outputPath).use { out => 
                  IO{ assigns.map{ instruction => 
                    out.write(instruction.binary16.toString) 
                    out.newLine()
                  }}
                }
            }.handleErrorWith{ (e: Throwable) => 
              IO.println(e.getMessage)
            } *> IO.println{
              val end = System.currentTimeMillis()
              s"Time: ${end - start}"
            }.as(ExitCode.Success)
          case _ => IO.println("Invalid input path.").as(ExitCode.Success)
      case None => IO.println("Not exist arguments.").as(ExitCode.Success)
