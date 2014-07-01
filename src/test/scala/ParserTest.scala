import Parser._
import Grammar._
import org.scalatest._


class ParserTest extends FlatSpec with Matchers {
  def get(x:Stream[(Tree,Stream[Token])]) = x.filter(_._2 == Stream()) match {
    case Seq() => Seq()
    case r @ _ => r.map(_._1)
  }
  def getFirst(x:Stream[(Tree,Stream[Token])]) = get(x) match {
    case Seq() => Seq()
    case r @ _ => r(0)
  }

  "The parser" should "handle regex" in {
    val g = new Grammar('Exp, Map(
      NonTerminal('Exp) -> Terminal("(on|off)")))
    val p = fromGrammar(g)

    getFirst(p.run(tokenize("on"))) should be (
      Branch('Exp,
          Stream(Leaf("on"))))

    getFirst(p.run(tokenize("off"))) should be (
      Branch('Exp,
          Stream(Leaf("off"))))

    getFirst(p.run(tokenize("onoff"))) should be (
      Stream())
  }

  it should "handle sequences" in {
    val g = new Grammar('Exp, Map(
      NonTerminal('Exp) -> Sequence(Terminal("a"),Terminal("b"))))
    val p = fromGrammar(g)

    getFirst(p.run(tokenize("a b"))) should be (
      Branch('Exp,
        Stream(Leaf("a"), Leaf("b"))))

    getFirst(p.run(tokenize("a"))) should be (
      Stream())

    getFirst(p.run(tokenize("b"))) should be (
      Stream())
  }

  it should "handle optional expressions" in {
    val g = new Grammar('Exp, Map(
      NonTerminal('Exp) -> Sequence(Option(Terminal("a")), Terminal("a"), Terminal("b"))))
    val p = fromGrammar(g)

    getFirst(p.run(tokenize("a b"))) should be (
      Branch('Exp,
        Stream(Leaf("a"), Leaf("b"))))
  }
}
