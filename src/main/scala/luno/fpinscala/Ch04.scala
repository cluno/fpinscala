package luno.fpinscala

object Ch04 {

  import scala.{Option => _, Either => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

  sealed trait Option[+A] {
    def isEmpty: Boolean
    def get: A

    def map[B](f: A => B): Option[B] = if(isEmpty) None else Some(f(get))
    def getOrElse[B>:A](default: => B): B = if(isEmpty) default else get
    def flatMap[B](f: A => Option[B]): Option[B] = if(isEmpty) None else f(get)
    def orElse[B>:A](ob: => Option[B]): Option[B] = if(isEmpty) ob else this
    def filter(f: A => Boolean): Option[A] = flatMap( a => if(f(a)) this else None )
  }

  case class Some[+A](a: A) extends Option[A] {
    def isEmpty = false
    def get = a
  }
  case object None extends Option[Nothing] {
    def isEmpty = true
    def get = sys.error("no such element")
  }

  object Option {
    import java.util.regex._

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

    /*
      EXERCISE 2: Implement the variance function (if the mean is m, variance is the mean of math.pow(x - m, 2), see definition) in terms of mean and flatMap.3
    */
    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs) flatMap { m =>
        mean(xs map { x => math.pow(x - m, 2) })
      }

    /*
      EXERCISE 3: Write a generic function map2, that combines two Option values using a binary function.
    */
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a flatMap ( x => b map (y => f(x, y)))

    def pattern(s: String): Option[Pattern] =
      try {
        Some(Pattern.compile(s))
      } catch {
        case e: PatternSyntaxException => None
      }

    def mkMatcher(pat: String): Option[String => Boolean] =
      pattern(pat) map (p => (s: String) => p.matcher(s).matches)

    /*
      EXERCISE 4: Re-implement bothMatch above in terms of this new function
    */
    def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
      map2(mkMatcher(pat1), mkMatcher(pat2))((f, g) => f(s) && g(s))

    /*
      EXERCISE 5: Write a function sequence, that combines a list of Options into one option containing a list of all the Some values in the original list.
    */
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight[Option[List[A]]](Some(Nil))((v, z) => for{ vv <- v;  zz <- z } yield (vv :: zz) )

    /*
      EXERCISE 6: Implement this function. It is straightforward to do using map and sequence, but try for a more efficient implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
    */
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a.foldRight[Option[List[B]]](Some(Nil))((v, z) => for{ vv <- f(v);  zz <- z } yield (vv :: zz) )
  }

  sealed trait Either[+E,+A] {
    /*
      EXERCISE 7: Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
    */
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(_) => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        a <- this
        bb <- b
      } yield f(a, bb)
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

    /*
      EXERCISE 8: Implement sequence and traverse for Either.
    */
    def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
      a.foldRight[Either[E, List[A]]](Right(Nil))((v, z) => for{ vv <- v; zz <- z } yield (vv :: zz) )

    def traverse[E, A, B](a: List[Either[E, A]])(f: A => Either[E, B]): Either[E, List[B]] = {
      a.foldRight[Either[E, List[B]]](Right(Nil))((v, z) =>
        for {vv <- v; vvv <- f(vv); zz <- z} yield (vvv :: zz))
    }
  }
}