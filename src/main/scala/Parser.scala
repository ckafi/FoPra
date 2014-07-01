import Grammar._

object Parser {

  type Token = String
  def tokenize(input:String):List[Token] = input.split("""\s+""").toList

  case class ParserMBuilder[A](run: List[Token] => List[(A,List[Token])]) extends ParserM[A]

  trait ParserM[A] {
    val run: List[Token] => List[(A,List[Token])]

    // bind
    def flatMap[B](f: A => ParserM[B]): ParserM[B] = ParserMBuilder(
      (input: List[Token]) => (for {
          (v,rest) <- (this run input)
      } yield f(v).run(rest)).flatten
    )

    def map[B](f: A => B): ParserM[B] = flatMap(x => ParserM(f(x)))

    def ++(m: ParserM[A]): ParserM[A] = ParserMBuilder(
      (input: List[Token]) => (this run input) ++ (m run input)
    )

    def +++(m: ParserM[A]): ParserM[A] = ParserMBuilder(
      (input: List[Token]) => (this ++ m) run input match {
        case Nil => Nil
        case x :: xs => List(x)
      }
    )
  }


  object ParserM {
    // return
    def apply[A](token: A)  = ParserMBuilder(
      (input: List[Token]) => List((token,input))
    )

    def zero[A](): ParserM[A] = ParserMBuilder(
      (input: List[Token]) => Nil
    )
  }


  object LeafParser extends ParserM[Tree] {
    val run = (list:List[Token]) => list match {
      case Nil => Nil
      case x :: xs => List((Leaf(x),xs))
    }
  }

  sealed abstract class Tree
  case class Leaf(content:String) extends Tree
  case class Branch(label:Symbol, children:List[Tree]) extends Tree



  def fromGrammar(g: Grammar): ParserM[Tree] = {

    def fromRule(r:Symbol):ParserM[Tree] = for {
      t <- fromRHS(g.rules(r))
    } yield Branch(r, t)


    def fromRHS(e: Exp):ParserM[List[Tree]] = e match {
      case NonTerminal(v) => ParserMBuilder(
        ((for {
          m <- fromRule(v)
        } yield List(m)) run _)
      )

      case Terminal(s) => ParserMBuilder(
        (list:List[Token]) => list match {
          case Nil => Nil
          case x :: xs => if(s == x) List((List(Leaf(s)),xs)) else Nil
        }
      )

      case Alternatives(xs @ _*) => xs.map(fromRHS).reduceLeft(_ ++ _)

      case Sequence(xs @ _*) => xs match {
        case Seq() => ParserM(List())
        case _ => for {
          m1 <- fromRHS(xs.head)
          m2 <- fromRHS(Sequence(xs.tail:_*))
        } yield m1 ++ m2
      }

      case Option(e) => fromRHS(e) ++ ParserM(List())

      case Plus(e) => ParserMBuilder(
        ((for {
            once <- fromRHS(e)
            rest <- fromRHS(Star(e))
          } yield (once ++ rest)) run _)
      )

      case Star(e) => fromRHS(Plus(e)) ++ ParserM(List())
    }

    fromRule(g.start.v)
  }
}
