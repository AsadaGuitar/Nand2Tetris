package assembler

import cats.*
import cats.data.*
import cats.implicits.*
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
import lib.syntax.StringSyntax.{_, given}
import lib.syntax.IntSyntax.{_, given}


object Main extends IOApp, ParserModule, SymbolTableModule:
  def run(args: List[String]) =
    val path = "./Test.f"    
    
    val bf: Resource[IO, BufferedReader] = 
      Resource.fromAutoCloseable(IO(new BufferedReader(new FileReader(path))))
    
    val assembly = bf.use( bf => IO(bf.lines.iterator().asScala.toList)).attempt

    IO.println("").as(ExitCode.Success)
