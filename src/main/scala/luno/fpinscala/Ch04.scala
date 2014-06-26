package luno.fpinscala

object Ch04 {

  import scala.{Option => _, Either => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = sys.error("todo")
    def getOrElse[B>:A](default: => B): B = sys.error("todo")
    def flatMap[B](f: A => Option[B]): Option[B] = sys.error("todo")
    def orElse[B>:A](ob: => Option[B]): Option[B] = sys.error("todo")
    def filter(f: A => Boolean): Option[A] = sys.error("todo")
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def failingFn(i: Int): Int = {
      val y: Int = throw new Exception("fail!")
      try {
        val x = 42 + 5
        x + y
      }
      catch { case e: Exception => 43 }
    }

    def failingFn2(i: Int): Int = {
      try {
        val x = 42 + 5
        x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      }
      catch { case e: Exception => 43 }
    }

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    def variance(xs: Seq[Double]): Option[Double] = sys.error("todo")

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = sys.error("todo")

    def sequence[A](a: List[Option[A]]): Option[List[A]] = sys.error("todo")

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
  }

  sealed trait Either[+E,+A] {
    def map[B](f: A => B): Either[E, B] = sys.error("todo")
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = sys.error("todo")
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = sys.error("todo")
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = sys.error("todo")
  }
  case class Left[+E](get: E) extends Either[E,Nothing]
  case class Right[+A](get: A) extends Either[Nothing,A]

  object Either {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)

    def safeDiv(x: Int, y: Int): Either[Exception, Int] =
      try Right(x / y)
      catch { case e: Exception => Left(e) }

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e) }
  }
}