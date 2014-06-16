package luno.fpinscala

object Ch03 {

  sealed trait List[+A] // `List` data type, parameterized on a type, `A`
  case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
  case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object List {

    def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

//    val example = Cons(1, Cons(2, Cons(3, Nil)))
//    val example2 = List(1,2,3)
//    val total = sum(example)

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

    // EXERCISE 1: What will the result of the following match expression be?
    //
    //    val x = List(1,2,3,4,5) match {
    //      case Cons(x, Cons(2, Cons(4, _))) => x
    //      case Nil => 42
    //      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    //      case Cons(h, t) => h + sum(t)
    //      case _ => 101
    //    }

    // EXERCISE 2: Implement the function tail for "removing" the first element
    // of a List. Notice the function takes constant time. What are different choices you
    // could make in your implementation if the List is Nil? We will return to this
    // question in the next chapter
    def tail[A](l: List[A]): List[A] = l match {
      case Cons(x, xs) => xs
      case _ => Nil
    }

    // EXERCISE 3: Generalize tail to the function drop, which removes the first
    // n elements from a list.
    def drop[A](l: List[A], n: Int): List[A] = sys.error("todo")

    // EXERCISE 4: Implement dropWhile,10 which removes elements from the
    // List prefix as long as they match a predicate.
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = sys.error("todo")

    // EXERCISE 5: Using the same idea, implement the function setHead for
    // replacing the first element of a List with a different value
    def setHead[A](l: List[A], h: A): List[A] = sys.error("todo")

    // EXERCISE 6: Not everything works out so nicely. Implement a function,
    // init, which returns a List consisting of all but the last element of a List.
    def init[A](l: List[A]): List[A] = sys.error("todo")

    // EXERCISE 9: Compute the length of a list using foldRight.
    def length[A](l: List[A]): Int = sys.error("todo")

    // EXERCISE 10: foldRight is not tail-recursive and will StackOverflow
    // for large lists. Convince yourself that this is the case, then write another general
    // list-recursion function, foldLeft that is tail-recursive, using the techniques we
    // discussed in the previous chapter.
    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")

    // EXERCISE 18: Write a function map, that generalizes modifying each element
    // in a list while maintaining the structure of the list.
    def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
  }

}
