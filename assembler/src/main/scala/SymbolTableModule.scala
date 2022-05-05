import cats.implicits.catsSyntaxEq

object SymbolTableModule {

  case class Label(name: String, address: Int)
  case class Variable(name: String, address: Int)
}

trait SymbolTableModule {
  import SymbolTableModule._
  import AssemblyRegex._
  import Utility._

  val assignAddress: Seq[String] => Seq[String] = (assembly: Seq[String]) => {

    val labels = {
      var count = -1
      assembly.foldLeft(Nil: Seq[Label]){ (acc, line) =>
        count += 1
        if (labelPattern.matches(line)) acc :+ Label(inner(line), count)
        else acc
      }
    }

    val variables = {
      var count = 1023
      assembly.filter { line =>
        aCommandPattern.matches(line) &&
          !labels.exists(_.name === line.tail)
      }.map { line =>
        val symbol = line.tail
        Variable(symbol, {
          symbol match {
            case "SP"     => 0
            case "LCL"    => 1
            case "ARG"    => 2
            case "THIS"   => 3
            case "THAT"   => 4
            case "SCREEN" => 16384
            case "KBD"    => 24576
            case registerPattern(_) => symbol.tail.toInt
            case numberPattern(_)   => symbol.toInt
            case _ => count += 1; count
          }
        })
      }
    }

    assembly.filter(ln => !labelPattern.matches(ln)).map {
      case line if aCommandPattern.matches(line) =>
        labels.find(_.name === line.tail).fold {
          val address = variables.find(_.name === line.tail).get.address.toString
          '@' + address
        } { label =>
          '@' + label.address.toString
        }
      case line => line
    }
  }
}
