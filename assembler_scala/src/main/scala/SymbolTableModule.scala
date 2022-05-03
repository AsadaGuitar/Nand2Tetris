import cats.implicits.catsSyntaxEq

object SymbolTableModule {

  abstract class Symbol(val name: String, val address: Int)
  case class Label(override val name: String, override val address: Int) extends Symbol(name,address)
  case class Variable(override val name: String, override val address: Int) extends Symbol(name,address)
}

trait SymbolTableModule {
  import SymbolTableModule._
  import AssemblyRegex._
  import Utility._

  def assignAddress(assembly: Seq[String]): Seq[String] ={

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
        aCommandPattern.matches(line) && !labels.exists(_.name === line.tail)
      }.map { line =>
        val symbol = line.tail
        if (numberPattern.matches(symbol)) {
          Variable(symbol,symbol.toInt)
        } else {
          count += 1
          Variable(line.tail, count)
        }
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
