package luno.fpinscala

import org.specs2.{Specification, ScalaCheck}
import org.scalacheck.{Gen, Prop, Arbitrary}

import Ch02._

class Ch02Spec extends Specification with ScalaCheck {
  def fib2(n: Int): Int =
    if(n < 2) n
    else fib2(n - 1) + fib2(n - 2)

  def is = "fib" ! check {
    Prop.forAll( Gen.chooseNum(1, 30) ) {
      n: Int => fib(n) == fib2(n)
    }
  } ^ "isSorted" ! check {
    Prop.forAll( Arbitrary.arbitrary[List[Int]] ) {
      xs: List[Int] => isSorted(xs.toArray, (a: Int, b: Int) => a > b) == (xs.sorted == xs)
    }
  }
}

