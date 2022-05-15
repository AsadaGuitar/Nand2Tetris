import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

class ParserModuleSpec extends AnyFlatSpec with ParserModule with Diagrams  with PrivateMethodTester:

  val symbolParser: PrivateMethod[Parser[String]] = PrivateMethod[Parser[String]](Symbol("symbolParser"))
  val numberParser: PrivateMethod[Parser[String]] = PrivateMethod[Parser[String]](Symbol("numberParser"))


  /**
   * labelParser: 'success pattern
   */
  "labelParser" should "Can extract the jump destination of hack assembler." in {
    assert(parseAll(labelParser, "(HELLO)").successful)
    assert(parseAll(labelParser, "(Hello)").successful)
    assert(parseAll(labelParser, "(hello)").successful)
    assert(parseAll(labelParser, "(Hello5)").successful)
    assert(parseAll(labelParser, "(hello5)").successful)
    assert(parseAll(labelParser, "(He5llo)").successful)
    assert(parseAll(labelParser, "(he5llo)").successful)
  }
  /**
   * labelParser: 'failure pattern
   */
  it should "Fails if not enclosed in parentheses or if the first character is a number." in {
    assert(!parseAll(labelParser, " ").successful)
    assert(!parseAll(labelParser, "HELLO").successful)
    assert(!parseAll(labelParser, "(HELLO").successful)
    assert(!parseAll(labelParser, "HELLO)").successful)
    assert(!parseAll(labelParser, "()").successful)
    assert(!parseAll(labelParser, "(123)").successful)
    assert(!parseAll(labelParser, "(123HELLO)").successful)
    assert(!parseAll(labelParser, "((HELLO))").successful)
  }


