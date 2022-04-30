import cats.implicits.catsSyntaxEq

object SymbolTableModule {

  abstract class Symbol(val name: String, val address: Int)
  case class Label(override val name: String, override val address: Int) extends Symbol(name,address)
  case class Variable(override val name: String, override val address: Int) extends Symbol(name,address)

  implicit class RichSymbols(symbol: Seq[Symbol]) {
    def getAddressByName(name: String): String ={
      symbol.find(_.name === name).get.address.toString
    }
  }
}

trait SymbolTableModule extends Utility {
  import SymbolTableModule._
  import AssemblyRegex._

  def assignAddress(assembly: Seq[String]): Seq[String] = {

    val labels = {
      var count = 0
      var labels: Seq[Label] = Seq()
      for (line <- assembly) yield {
        if (labelPattern.matches(line)) {
          labels = labels :+ Label(line.inner, count)
        }
        count += 1
      }
      labels
    }
    val variables = {
      var count = 15
      assembly.filter { line =>
        aCommandPattern.matches(line) && !labels.exists(_.name === line.tail)
      }.map { line =>
        count += 1
        Variable(line.tail, count)
      }
    }

    assembly.filter(ln => !labelPattern.matches(ln)).map {
      // A command
      case line if aCommandPattern.matches(line) =>
        labels.find(_.name === line.tail).fold {
          '@' + variables.getAddressByName(line.tail)
        } { label =>
          '@' + label.address.toString
        }
      // C command
      case line => line
    }
  }
}
