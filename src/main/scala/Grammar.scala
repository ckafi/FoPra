object Grammar {
  sealed abstract class Exp

  case class NonTerminal(v:Symbol) extends Exp
  case class Terminal(v:String) extends Exp

  case class Option(v:Exp) extends Exp
  case class Star(v:Exp) extends Exp
  case class Plus(v:Exp) extends Exp

  case class Alternatives(v:Exp*) extends Exp {
    override def toString = "Alternatives(" + v.mkString(", ") + ")"
  }
  case class Sequence(v:Exp*) extends Exp {
    override def toString = "Sequence(" + v.mkString(", ") + ")"
  }

  class Grammar(val start:NonTerminal, val rules:Map[NonTerminal, Exp]) {
    override def toString = rules.mkString("\n")
  }

  implicit def symbol2NonTerminal(s:Symbol) = new NonTerminal(s)
  implicit def string2Terminal(s:String) = new Terminal(s)
}
