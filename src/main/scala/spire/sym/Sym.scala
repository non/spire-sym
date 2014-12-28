package spire
package sym

import spire.algebra._
import spire.implicits._
import spire.math.{Algebraic => A, Rational}

sealed abstract class Sym { lhs =>

  import Sym._

  def partialSignum: Double =
    this.evaluate.fold(Double.NaN)(_.signum.toDouble)

  def partialCompare(rhs: Sym): Double =
    (lhs - rhs).partialSignum

  def ===(rhs: Sym): Boolean = partialCompare(rhs) == 0.0
  def =!=(rhs: Sym): Boolean = partialCompare(rhs) != 0.0
  def <(rhs: Sym): Boolean = partialCompare(rhs) < 0.0
  def <=(rhs: Sym): Boolean = partialCompare(rhs) <= 0.0
  def >(rhs: Sym): Boolean = partialCompare(rhs) > 0.0
  def >=(rhs: Sym): Boolean = partialCompare(rhs) >= 0.0

  def +(rhs: Sym): Sym =
    if (lhs == rhs) lhs * Two
    else if (rhs.isZero) lhs
    else if (lhs.isZero) rhs
    else (lhs, rhs) match {
      case (Const(n1), Const(n2)) => Const(n1 + n2)
      case (Const(n1), Sum(xs2, c2)) => Sum(xs2, n1 + c2)
      case (Sum(xs1, c1), Const(n2)) => Sum(xs1, n2 + c1)
      case (Const(n1), _) => Sum(rhs :: Nil, n1)
      case (_, Const(n2)) => Sum(lhs :: Nil, n2)
      case (Sum(xs1, c1), Sum(xs2, c2)) => Sum(xs1 ::: xs2, c1 + c2).simplify
      case _ => Sum(lhs :: rhs :: Nil).simplify
    }

  def unary_-(): Sym =
    this match {
      case Const(n) => Const(-n)
      case Sum(xs, c) => Sum(xs.map(-_), -c)
      case Product(xs, c) => Product(xs, -c).simplify
      case _ => this * MinusOne
    }

  def -(rhs: Sym): Sym =
    (lhs + (-rhs)).simplify

  def *(rhs: Sym): Sym =
    if (lhs == rhs) lhs ** Two
    else if (lhs.isZero || rhs.isZero) Zero
    else if (lhs.isOne) rhs
    else if (rhs.isOne) lhs
    else (lhs, rhs) match {
      case (Const(n1), Const(n2)) => Const(n1 * n2)
      case (Const(n1), Sum(xs2, c2)) => Sum(xs2.map(_ * lhs), n1 * c2)
      case (Sum(xs1, c1), Const(n2)) => Sum(xs1.map(_ * rhs), n2 * c1)
      case (Const(n1), Product(xs2, c2)) => Product(xs2, n1 * c2)
      case (Product(xs1, c1), Const(n2)) => Product(xs1, n2 * c1)
      case (Const(n1), _) => Product(rhs :: Nil, n1)
      case (_, Const(n2)) => Product(lhs :: Nil, n2)
      case (Product(xs1, c1), Product(xs2, c2)) => Product(xs1 ::: xs2, c1 * c2).simplify
      case _ => Product(lhs :: rhs :: Nil).simplify
    }

  def reciprocal(): Sym =
    this match {
      case Const(n) => Const(n.reciprocal)
      case Power(x, k) => x ** (-k)
      case _ => Power(this, MinusOne)
    }

  def /(rhs: Sym): Sym =
    if (lhs == rhs) One
    else if (lhs.isZero) Zero
    else if (rhs.isZero) throw new ArithmeticException("/0")
    else if (lhs.isOne) rhs ** MinusOne
    else if (rhs.isOne) lhs
    else (lhs * rhs).simplify

  def pow(k: Sym): Sym =
    this ** k

  def **(k: Sym): Sym =
    if (k.isZero) One
    else if (k.isOne) this
    else (this, k) match {
      case (Const(x), Const(j)) =>
        if (j.isWhole) {
          val n = j.toInt // ugh
          if (n > 0) Const(x.pow(n))
          else if (n == 0) One
          else Const(x.reciprocal.pow(-n))
        } else {
          Power(this, k)
        }
      case (Power(x, j), _) =>
        val l = j * k
        l match {
          case Const(r) =>
            if (r.isWhole) {
              val n = r.toInt
              if (n == 0) One
              else if (n == 1) x
              else x ** l
            } else {
              x ** l
            }
          case _ =>
            x ** l
        }
      case (Product(xs, c), Const(j)) =>
        Product(xs.map(_ ** k), c.pow(j.toInt)).simplify
      case (Product(xs, c), _) =>
        Product((Sym(c) ** k) :: xs.map(_ ** k)).simplify
      case _ =>
        Power(this, k)
    }

  def sqrt: Sym = nroot(2)

  def nroot(k: Int): Sym =
    this match {
      case Const(r) =>
        Sym.exactNRoot(r, k) match {
          case Some(r) => Const(r)
          case None => this ** Sym(1, k)
        }
      case Product(xs, c) =>
        Sym.exactNRoot(c, k) match {
          case Some(r) => Product(xs.map(_ nroot k), r).simplify
          case None => Product((Const(c) nroot k) :: xs.map(_ nroot k)).simplify
        }
      case Power(x, k0) =>
        x ** (k0 / Sym(k))
      case _ =>
        this ** Sym(1, k)
    }

  def isZero: Boolean =
    this match {
      case Zero => true
      case _ => false
    }

  def isOne: Boolean =
    this match {
      case One => true
      case _ => false
    }

  def hasUnboundNames: Boolean =
    this match {
      case Const(_) => false
      case Name(_) => true
      case Sum(syms, _) => syms.exists(_.hasUnboundNames)
      case Product(syms, _) => syms.exists(_.hasUnboundNames)
      case Power(x, k) => x.hasUnboundNames || k.hasUnboundNames
    }

  def subst(names: (String, Sym)*): Sym =
    subst(names.toMap)

  def subst(names: Map[String, Sym]): Sym =
    this match {
      case Const(_) => this
      case Name(s) => names.getOrElse(s, this)
      case Sum(syms, c) => Sum(syms.map(_.subst(names)), c).simplify
      case Product(syms, c) => Product(syms.map(_.subst(names)), c).simplify
      case Power(x, k) => Power(x.subst(names), k.subst(names)).simplify
    }

  def evaluate: Option[A] = {
    def evs(curr: A, syms: List[Sym])(f: (A, A) => A): Option[A] =
      syms match {
        case h :: t => h.evaluate match {
          case Some(n) => evs(f(curr, n), t)(f)
          case None => None
        }
        case Nil => Some(curr)
      }
  
    this match {
      case Const(r) =>
        Some(A(r))
      case Name(_) =>
        None
      case Sum(syms, c) =>
        evs(A(c), syms)(_ + _)
      case Product(syms, c) =>
        evs(A(c), syms)(_ * _)
      case Power(x, Const(r)) =>
        val (n, d) = (r.numerator.toInt, r.denominator.toInt)
        x.evaluate.map(_.pow(n).nroot(d))
      case Power(_, _) =>
        sys.error("!!")
    }
  }

  def simplify: Sym =
    this match {
      case Const(_) => this
      case Name(_) => this
      case Sum(syms, c) => simplifySum(syms, c)
      case Product(syms, c) => simplifyProduct(syms, c)
      case Power(x, k) => x.simplify ** k.simplify //TODO
    }

  def simplifyPower(sym: Sym, n: Sym): Sym = {
    val s0 = sym.simplify
    val n0 = n.simplify
    (s0, n0) match {
      case (Const(x), Const(k)) =>
        if (!k.isWhole) Power(s0, n0)
        else if (k > 0) Const(x.pow(k.toInt))
        else if (k < 0) Power(Const(x.pow(k.toInt)), MinusOne)
        else One
      case (Product(xs, c), _) =>
        Product((Const(c) ** n0) :: xs.map(_ ** n0)).simplify
      case (Power(x, k), _) =>
        Power(x, k * n0).simplify
      case _ =>
        Power(s0, n0)
    }
  }

  def simplifySum(syms: List[Sym], c: Rational): Sym = {
    def addSym(m: Map[Sym, Sym], sym: Sym): Map[Sym, Sym] =
      sym match {
        case Sum(xs, c) => xs.foldLeft(m)(addSym).updated(Const(c), m(Const(c)) + One)
        case Product(x :: Nil, c) => m.updated(x, m(x) + Const(c))
        case x => m.updated(x, m(x) + One)
      }
    simplifyCombined(syms, c)(_ + _)(_ * _)(Sum)(addSym)
  }

  def simplifyProduct(syms: List[Sym], c: Rational): Sym = {
    def multSym(m: Map[Sym, Sym], sym: Sym): Map[Sym, Sym] =
      sym match {
        case Product(xs, c) => xs.foldLeft(m)(multSym).updated(Const(c), m(Const(c)) + One)
        case Power(x, k) => m.updated(x, m(x) + k)
        case x => m.updated(x, m(x) + One)
      }
    simplifyCombined(syms, c)(_ * _)(_ ** _)(Product)(multSym)
  }

  def simplifyCombined(syms: List[Sym], c: Rational)
    (f: (Rational, Rational) => Rational)
    (g: (Sym, Sym) => Sym)
    (builder: (List[Sym], Rational) => Sym)
    (adder: (Map[Sym, Sym], Sym) => Map[Sym, Sym]): Sym = {
  
    val e = Map.empty[Sym, Sym].withDefaultValue(Zero)
    val seen = syms.map(_.simplify).foldLeft(e)(adder)

    val terms = seen.flatMap {
      case (_, Zero) => None
      case (sym, One) => Some(sym)
      case (sym, b) => Some(g(sym, b))
    }.toList

    val (c2, ts2) = terms.foldLeft((c, List.empty[Sym])) {
      case ((c2, ts2), Const(n)) => (f(c2, n), ts2)
      case ((c2, ts2), sym) => (c2, sym :: ts2)
    }

    if (ts2.nonEmpty) builder(ts2, c2) else Const(c2)
  }


}

object Sym {
  implicit def apply(n: Int): Sym = Const(Rational(n))
  def apply(n: BigInt): Sym = Const(Rational(n))
  implicit def apply(n: Rational): Sym = Const(n)
  def apply(n: BigInt, d: BigInt): Sym = Const(Rational(n, d))
  implicit def apply(c: Char): Sym = Name(c.toString)
  def apply(name: String): Sym = Name(name)

  sealed abstract class Atom extends Sym

  case class Const(r: Rational) extends Atom {
    override def toString: String = r.toString
  }

  case class Name(name: String) extends Atom {
    override def toString: String = name
  }

  case class Sum(xs: List[Sym] = Nil, const: Rational = Rational.zero) extends Sym
  case class Product(xs: List[Sym] = Nil, const: Rational = Rational.one) extends Sym
  case class Power(x: Sym, k: Sym) extends Sym

  implicit val symAlgebra = new SymAlgebra

  val MinusOne: Sym = Const(Rational(-1))
  val Zero: Sym = Const(Rational.zero)
  val One: Sym = Const(Rational.one)
  val Two: Sym = Const(Rational(2))

  class SymAlgebra extends Field[Sym] with NRoot[Sym] with PartialOrder[Sym] with Serializable {
  
    def partialCompare(x: Sym, y: Sym): Double =
      x partialCompare y
    
    def zero: Sym = Zero
    
    def one: Sym = One
    
    def plus(a: Sym, b: Sym): Sym = a + b
    def negate(a: Sym): Sym = -a
    def times(a: Sym, b: Sym): Sym = a * b
    def quot(a: Sym, b: Sym): Sym = a / b
    def mod(a: Sym, b: Sym): Sym = Zero
    override def reciprocal(a: Sym): Sym = a.reciprocal
    def div(a: Sym, b: Sym): Sym = a / b
    override def fromInt(n: Int): Sym = Sym(n)
    override def pow(s: Sym, k: Int): Sym = s ** Sym(k)
    def nroot(s: Sym, k: Int): Sym = s nroot k
    def fpow(s: Sym, k: Sym): Sym = s ** k

    def gcd(a: Sym, b: Sym): Sym = a //fixme
  }

  def exactNRoot(x: BigInt, k: Int): Option[BigInt] = {
    val d = x.nroot(k)
    if (d.pow(k) == x) Some(d) else None
  }

  def exactNRoot(r: Rational, k: Int): Option[Rational] =
    (exactNRoot(r.numerator, k), exactNRoot(r.denominator, k)) match {
      case (Some(n), Some(d)) => Some(Rational(n, d))
      case _ => None
    }
}
