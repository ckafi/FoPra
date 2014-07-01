import Grammar._

object Parser {

  type Token = String
  def tokenize(input:String):Stream[Token] = Stream.empty ++ input.split("""\s+""")

  case class ParserMBuilder[A](run: Stream[Token] => Stream[(A,Stream[Token])]) extends ParserM[A]

  trait ParserM[A] {
    val run: Stream[Token] => Stream[(A,Stream[Token])]

    // bind
    def flatMap[B](f: A => ParserM[B]): ParserM[B] = ParserMBuilder(
      (input: Stream[Token]) => (for {
          (v,rest) <- (this run input)
      } yield f(v).run(rest)).flatten
    )

    def map[B](f: A => B): ParserM[B] = flatMap(x => ParserM(f(x)))

    def ++(m: ParserM[A]): ParserM[A] = ParserMBuilder(
      (input: Stream[Token]) => (this run input) ++ (m run input)
    )

    def +++(m: ParserM[A]): ParserM[A] = ParserMBuilder(
      (input: Stream[Token]) => (this ++ m) run input match {
        case Seq() => Stream.empty
        case x #:: xs => Stream(x)
      }
    )
  }


  object ParserM {
    // return
    def apply[A](token: A)  = ParserMBuilder(
      (input: Stream[Token]) => Stream((token,input))
    )

    def zero[A](): ParserM[A] = ParserMBuilder(
      (input: Stream[Token]) => Stream.empty
    )
  }


  sealed abstract class Tree
  case class Leaf(content:String) extends Tree
  case class Branch(label:Symbol, children:Stream[Tree]) extends Tree


  def fromGrammar(g: Grammar): ParserM[Tree] = {

    def fromRule(r:Symbol):ParserM[Tree] = for {
      t <- fromRHS(g.rules(r))
    } yield Branch(r, t)


    def fromRHS(e: Exp):ParserM[Stream[Tree]] = e match {
      case NonTerminal(v) => ParserMBuilder(
        ((for {
          m <- fromRule(v)
        } yield Stream(m)) run _)
      )

      case Terminal(s) => ParserMBuilder(
        (list:Stream[Token]) => list match {
          case Seq() => Stream.empty
          case x #:: xs => if(x matches s) Stream((Stream(Leaf(x)),xs)) else Stream.empty
        }
      )

      case Alternatives(xs @ _*) => xs.map(fromRHS).reduceLeft(_ ++ _)

      case Sequence(xs @ _*) => xs match {
        case Seq() => ParserM(Stream())
        case _ => for {
          m1 <- fromRHS(xs.head)
          m2 <- fromRHS(Sequence(xs.tail:_*))
        } yield m1 ++ m2
      }

      case Option(e) => fromRHS(e) ++ ParserM(Stream())

      case Plus(e) => ParserMBuilder(
        ((for {
            once <- fromRHS(e)
            rest <- fromRHS(Star(e))
          } yield (once ++ rest)) run _)
      )

      case Star(e) => fromRHS(Plus(e)) ++ ParserM(Stream())
    }

    fromRule(g.start.v)
  }
}
