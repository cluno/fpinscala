package luno.fpinscala

import org.specs2.{Specification, ScalaCheck}
import org.scalacheck._

import Ch03._
import Ch03.{List => CList}
import CList._

import scala.collection.immutable.{List => NList}
import scala.collection.immutable.{Nil => NNil}

class Ch03Spec extends Specification with ScalaCheck {

  val list: Gen[NList[Int]] = Gen.listOf(Gen.choose(0, 1000))

  def toCList[A](l: NList[A]): CList[A] = l match {
    case h :: t => Cons(h, toCList(t))
    case _ => Nil
  }

  def toNList[A](l: CList[A]): NList[A] = l match {
    case Cons(h, t) => h :: toNList(t)
    case _ => NNil
  }

  def head2[A](l: CList[A]): A = l match {
    case Cons(h, t) => h
    case _ => throw new NoSuchElementException("head of empty list")
  }

  def is = "tail" ! check {
    Prop.forAll( Arbitrary.arbitrary[NList[Int]] suchThat ( _.length > 2) ) {
      xs: NList[Int] =>
        xs.tail.head == head2(tail(toCList(xs)))
    }
  } ^ "dropWhile" ! check {
    Prop.forAll(list suchThat (_.length > 5)) {
      xs: NList[Int] =>
        val f = (a: Int) => a % 2 != 0
        xs.dropWhile(f) == toNList(dropWhile(toCList(xs), f))
    }
  } ^ "setHead" ! check {
    Prop.forAll(list suchThat (_.length > 5)) {
      xs: NList[Int] =>
        val h = scala.util.Random.nextInt
        val cs = toCList(xs)
        h :: xs.tail == toNList(setHead(cs, h))
    }
  } ^ "init" ! check  {
    Prop.forAll(list suchThat (_.length > 5)) {
      xs: NList[Int] =>
        xs.init == toNList(init(toCList(xs)))
    }
  } ^ "length" ! check {
    Prop.forAll(list suchThat (_.length > 5)) {
      xs: NList[Int] =>
        xs.length  == CList.length(toCList(xs))
    }
  } ^ "foldLeft" ! check {
    Prop.forAll(list suchThat (_.length > 5)) {
      xs: NList[Int] =>
        xs.foldLeft(1)(_ + _) == foldLeft(toCList(xs), 1)(_ + _)
    }
  }
}