import Parser._
import Grammar._

object Test {
  val t = new Grammar('Digit, Map(
    NonTerminal('Digit) -> Star(Alternatives("0","1","2","3","4","5","6","7","8","9"))
  ))

  val p = fromGrammar(t)


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

  // val ae = new Grammar('Exp, Map(
  //   NonTerminal('Exp) -> Alternatives('Parenthesized, 'Num, 'Id, 'Mul, 'Add),
  //   NonTerminal('Parenthesized) -> Alternatives(Sequence("(",'Exp,")")),
  //
  //   NonTerminal('Num) -> Alternatives(Plus('Digit)),
  //   NonTerminal('Id) -> Alternatives(Sequence('Char,Star(Alternatives('Char,'Digit)))),
  //   // Ranges are not yet implemented
  //   NonTerminal('Digit) -> Alternatives("0","1","2","3","4","5","6","7","8","9"),
  //   NonTerminal('Char) -> Alternatives("a","b","c"), // you get the gist
  //
  //   NonTerminal('Mul) -> Alternatives(Sequence('Multiplicand, "*", 'Multiplicand)),
  //   NonTerminal('Multiplicand) -> Alternatives('Parenthesized, 'Num, 'Id),
  //
  //   NonTerminal('Add) -> Alternatives(Sequence('Summand, "+", 'Summand)),
  //   NonTerminal('Summand) -> Alternatives('Parenthesized, 'Num, 'Id, 'Mul)
  // ))
}
