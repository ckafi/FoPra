import Parser._
import Grammar._
import org.scalatest._


class AESpec extends FlatSpec with Matchers {

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
    NonTerminal('Id) -> Sequence('Char,Star(Alternatives('Char,'Digit))),
    // Ranges are not yet implementeddd
    NonTerminal('Char) -> Alternatives("[A-Za-z][A-Za-z0-9_]*"),

    NonTerminal('Mul) -> Sequence('Multiplicand, "\\*", 'Multiplicand),
    NonTerminal('Multiplicand) -> Alternatives('Parenthesized, 'Num, 'Id),

    NonTerminal('Add) -> Sequence('Summand, "\\+", 'Summand),
    NonTerminal('Summand) -> Alternatives('Parenthesized, 'Num, 'Id, 'Mul)
  ))

  val p = fromGrammar(ae)

  def get(x:List[(Tree,List[Token])]) = x.filter(_._2 == List())(0)._1

  "The parser" should "should be able to parse single digits" in {
    get(p.run(tokenize("1"))) should be (
      Branch('Exp,
        List(Branch('Num,
          List(Leaf("1"))))))
  }
}
