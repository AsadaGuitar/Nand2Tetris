import cats.data.Kleisli
import cats.implicits._

object ArithmeticCommand {

  type BF = (Int,Int) => Any
}

trait ArithmeticCommand {
  import ArithmeticCommand._

  val binaryFunction: Kleisli[Option,String,BF] = Kleisli[Option,String,BF]{
    case "add" => ((_: Int) + (_: Int)).some
    case "sub" => ((_: Int) - (_: Int)).some
    case "eq" => ((_: Int) === (_: Int)).some
    case "gt" => ((_: Int) > (_: Int)).some
    case "lt" => ((_: Int) < (_: Int)).some
    case "and" => ((_: Int) & (_: Int)).some
    case "or" => ((_: Int) | (_: Int)).some
    case _ => None
  }
}
