import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

class ParserModuleSpec extends AnyFlatSpec with ParserModule with Diagrams  with PrivateMethodTester:

  "symbolParser" should "" in {
    assert(parseAll(symbolParser, "HELLO").successful)
  }

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

  /**
   * commandAParser: 'success pattern
   */
  "commandAParser" should "Can extract 'A instruction' of hack assembler." in {
    assert(parseAll(commandAParser, "@HELLO").successful)
    assert(parseAll(commandAParser, "@Hello").successful)
    assert(parseAll(commandAParser, "@hello").successful)
    assert(parseAll(commandAParser, "@HELLO5").successful)
    assert(parseAll(commandAParser, "@Hello5").successful)
    assert(parseAll(commandAParser, "@hello5").successful)
    assert(parseAll(commandAParser, "@HEL5LO").successful)
    assert(parseAll(commandAParser, "@Hel5lo").successful)
    assert(parseAll(commandAParser, "@hel5lo").successful)
    assert(parseAll(commandAParser, "@.HEL:LO_").successful)
    assert(parseAll(commandAParser, "@:He_llo.").successful)
    assert(parseAll(commandAParser, "@_hel.lo:").successful)
    assert(parseAll(commandAParser, "@1").successful)
    assert(parseAll(commandAParser, "@123").successful)
  }
  /**
   * commandAParser: 'failure pattern
   */
  it should "Fails if leading @ is not present or if a number is entered after @." in {
    assert(!parseAll(commandAParser, " ").successful)
    assert(!parseAll(commandAParser, "@").successful)
    assert(!parseAll(commandAParser, "HELLO").successful)
    assert(!parseAll(commandAParser, "@5HELLO").successful)
    assert(!parseAll(commandAParser, "@HE@LLO").successful)
    assert(!parseAll(commandAParser, "@HELLO@").successful)
    assert(!parseAll(commandAParser, "HEL@LO").successful)
    assert(!parseAll(commandAParser, "@@").successful)
  }


