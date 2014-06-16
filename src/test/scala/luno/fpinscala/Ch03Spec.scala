package luno.fpinscala

import org.specs2.{Specification, ScalaCheck}
import org.scalacheck._

import Gen._
import Arbitrary.arbitrary

import Ch03._
import Ch03.{List => CList}
import scala.collection.immutable.{List => NList }

class Ch03Spec extends Specification with ScalaCheck {

  def toCList[A](l: NList[A]): CList[A] = l match {
     case h :: t => Cons(h, toCList(t))
     case _ => Nil
  }

  def head[A](l: CList[A]): A = l match {
    case Cons(h, t) => h
    case _ =>  throw new NoSuchElementException("head of empty list")
  }

  def is = "tail" ! check {
    Prop.forAll( Arbitrary.arbitrary[NList[Int]] suchThat ( _.length > 2) ) {
      xs: NList[Int] =>
        xs.tail.head == head(CList.tail(toCList(xs)))
    }
  }
}

