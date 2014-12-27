package spire.sym

import spire.math.Rational
import spire.laws.{OrderLaws, RingLaws}
import spire.laws.SpireArbitrary._
import org.typelevel.discipline.Predicate
import org.typelevel.discipline.scalatest.Discipline

import org.scalatest.FunSuite
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class LawTests extends FunSuite with Discipline {

  implicit val pred: Predicate[Sym] =
    new Predicate[Sym] { def apply(a: Sym) = a =!= Sym.Zero }

  implicit val arb: Arbitrary[Sym] =
    Arbitrary(arbitrary[Rational].map(Sym(_)))

  checkAll("Sym", OrderLaws[Sym].partialOrder)
  checkAll("Sym", RingLaws[Sym].field)
  // no laws for NRoot... yet!
}
