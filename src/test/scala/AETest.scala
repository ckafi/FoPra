import Parser._
import Grammar._
import org.scalatest._


class AETest extends FlatSpec with Matchers {
  def get(x:List[(Tree,List[Token])]) = x.filter(_._2 == List()) match {
    case Seq() => Seq()
    case r @ _ => r.map(_._1)
  }
  def getFirst(x:List[(Tree,List[Token])]) = get(x) match {
    case Seq() => Seq()
    case r @ _ => r(0)
  }

  /* Example: The grammar for plt/ae
  *
  * Exp ::= Parenthesized | Num | Id | Mul | Add
  * Parenthesized ::= ( Exp )
  *
  * Num ::= [0-9]+
  * Id ::= [A-Za-z][A-Za-z0-9_]*
  *
  * Mul ::= Multiplicand * Multiplicand
  * Multiplicand ::= Parenthesized | Num | Id
  *
  * Add ::= Summand + Summand
  * Summand ::= Parenthesized | Num | Id | Mul
  */

  val ae = new Grammar('Exp, Map(
    NonTerminal('Exp) -> Alternatives('Parenthesized, 'Num, 'Id, 'Mul, 'Add),
    NonTerminal('Parenthesized) -> Sequence("\\(",'Exp,"\\)"),

    NonTerminal('Num) -> Terminal("[0-9]+"),
    NonTerminal('Id) -> Terminal("[A-Za-z][A-Za-z0-9_]*"),

    NonTerminal('Mul) -> Sequence('Multiplicand, "\\*", 'Multiplicand),
    NonTerminal('Multiplicand) -> Alternatives('Parenthesized, 'Num, 'Id),

    NonTerminal('Add) -> Sequence('Summand, "\\+", 'Summand),
    NonTerminal('Summand) -> Alternatives('Parenthesized, 'Num, 'Id, 'Mul)
  ))

  val p = fromGrammar(ae)

  "The parser" should "parse single digits" in {
    getFirst(p.run(tokenize("1"))) should be (
      Branch('Exp, List(
        Branch('Num, List(
          Leaf("1"))))))
  }

  it should "parse multiple digits" in {
    getFirst(p.run(tokenize("123"))) should be (
      Branch('Exp, List(
        Branch('Num, List(
          Leaf("123"))))))
  }

  it should "parse alphanumeric characters without leading digit" in {
    getFirst(p.run(tokenize("abc32Def"))) should be (
      Branch('Exp, List(
        Branch('Id, List(
          Leaf("abc32Def"))))))

    getFirst(p.run(tokenize("1abc"))) should be (
      List())
  }

  it should "parse complex expressions" in {
    getFirst(p.run(tokenize("1 + 2 * ( 3 + 4 )"))) should be (
      Branch('Exp, List(
        Branch('Add, List(
            Branch('Summand, List(
              Branch('Num,List(Leaf("1"))))),
            Leaf("+"),
            Branch('Summand, List(
              Branch('Mul,List(
                Branch('Multiplicand,List(
                  Branch('Num,List(Leaf("2"))))),
                Leaf("*"),
                Branch('Multiplicand,List(
                  Branch('Parenthesized,List(
                    Leaf("("),
                    Branch('Exp,List(
                      Branch('Add,List(
                        Branch('Summand,List(
                          Branch('Num,List( Leaf("3"))))),
                          Leaf("+"),
                          Branch('Summand,List(
                            Branch('Num,List(Leaf("4"))))))))),
                          Leaf(")"))))))))))))))
  }
}
