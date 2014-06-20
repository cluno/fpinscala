package luno.fpinscala

import scala.annotation.tailrec

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

    // foldRight(Cons(a, Nil), z)(f) ----> f(a, z)
    // foldRight(Cons(a, Cons(b, Nil)), z)(f) ----> f(a, f(b, z))
    // foldRight(Cons(a, Cons(b, Cons(c, Nil))), z)(f) ----> f(a, f(b, f(c, z)))
    @tailrec
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
      as match {
        case Nil => z
//        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        case Cons(x, xs) => foldRight(xs, f(x, z))(f)
      }

    def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

    // EXERCISE 1: What will the result of the following match expression be? - 3
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
    def drop[A](l: List[A], n: Int): List[A] = {
      var ret = l
      var count = n
      while(count > 0){
        ret = tail(ret)
        count -= 1
      }
      ret
    }

    // EXERCISE 4: Implement dropWhile, which removes elements from the
    // List prefix as long as they match a predicate.
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      def head[A](l: List[A]): A = l match {
        case Cons(h, t) => h
        case _ => throw new NoSuchElementException("head of empty list")
      }

      @tailrec
      def go(l: List[A]): List[A] = {
       if(!f(head(l))) l
       else go(tail(l))
      }

      go(l)
    }

    // EXERCISE 5: Using the same idea, implement the function setHead for
    // replacing the first element of a List with a different value
    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => Cons(h, Nil)
      case Cons(c, cs) => Cons(h, cs)
    }

    // EXERCISE 6: Not everything works out so nicely. Implement a function,
    // init, which returns a List consisting of all but the last element of a List.
    def init[A](l: List[A]): List[A] = {
      def head[A](l: List[A]): A = l match {
        case Cons(h, t) => h
        case _ => throw new NoSuchElementException("head of empty list")
      }

      def go(l: List[A]): List[A] = l match {
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(head(l), go(tail(l)))
      }

      go(l)
    }

    // EXERCISE 7: Can product implemented using foldRight immediately halt the recursion and return 0.0
    // if it encounters a 0.0? Why or why not?
    //

    // EXERCISE 8: See what happens when you pass Nil and Cons themselves to
    // foldRight, like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    // What do you think this says about the relationship between foldRight and the data constructors of List?
    // List(1, 2, 3) = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) = Cons(1,Cons(2,Cons(3,Nil)))

    // EXERCISE 9: Compute the length of a list using foldRight.
    def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b + 1)

    // EXERCISE 10: foldRight is not tail-recursive and will StackOverflow
    // for large lists. Convince yourself that this is the case, then write another general
    // list-recursion function, foldLeft that is tail-recursive, using the techniques we
    // discussed in the previous chapter.

    // foldLeft(Cons(a, Nil), z)(f) ----> f(z, a)
    // foldLeft(Cons(a, Cons(b, Nil)), z)(f) ----> f(f(z, a), b)
    // foldLeft(Cons(a, Cons(b, Cons(c, Nil))), z)(f) ----> f(f(f(z, a), b), c)
    @tailrec
    def foldLeft[A,B](ns: List[A], z: B)(f: (B, A) => B): B =
      ns match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    // EXERCISE 11: Write sum, product, and a function to compute the length of a list using foldLeft.
    def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
    def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
    def length2[A](ns: List[A]): Int = foldLeft(ns, 0)((b, a) => b + 1)

    // EXERCISE 12: Write a function that returns the reverse of a list (so given
    // List(1,2,3) it returns List(3,2,1)). See if you can write it using a fold.
    // foldLeft(Cons(a, Cons(b, Cons(c, Nil))), z)(f) ----> f(f(f(z, a), b), c)
    def reverse[A](ns: List[A]): List[A] = foldLeft(ns, List[A]()) ((b, a) => Cons(a, b))

    // EXERCISE 13 (hard): Can you write foldLeft in terms of foldRight?
    // How about the other way around?
    def foldLeft2[A,B](ns: List[A], z: B)(f: (B, A) => B): B = ???

    // EXERCISE 14: Implement append in terms of either foldLeft or foldRight.
    def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a1, a2)((b, a) => Cons(a, b))

    // EXERCISE 15 (hard): Write a function that concatenates a list of lists into a
    // single list. Its runtime should be linear in the total length of all lists.
    // Try to use functions we have already defined.
    def concat[A](ns: List[List[A]]): List[A] = foldLeft(ns, List[A]())((b, a) => append(b, a))

    // EXERCISE 16: Write a function that transforms a list of integers by adding 1
    // to each element.
    // (Reminder: this should be a pure function that returns a new List!)
    def transfer(ns: List[Int]): List[Int] = foldLeft(ns, List[Int]())((b, a) => Cons(a + 1, b))

    // EXERCISE 17: Write a function that turns each value in a List[Double] into a String.
    def toStringFromDouble(xs: List[Double]): List[String] =
      foldLeft(xs, List[String]())((b, a) => Cons(a.toString, b))

    // EXERCISE 18: Write a function map, that generalizes modifying each element
    // in a list while maintaining the structure of the list.
    def map[A,B](l: List[A])(f: A => B): List[B] = foldLeft(l, List[B]())((b, a) => Cons(f(a), b))

    // EXERCISE 19: Write a function filter that removes elements from a list
    // unless they satisfy a given predicate. Use it to remote all odd numbers from a List[Int].
    def filter[A](l: List[A])(p: A => Boolean): List[A] =
      foldLeft(l, List[A]())((b, a) => if(p(a)) Cons(a, b) else Nil)

    // EXERCISE 20: Write a function flatMap, that works like map except that
    // the function given will return a list instead of a single result, and that list should be
    // inserted into the final resulting list. Here is its signature:
    // List(List(0, 1, 2), List(3, 4, 5), List(6, 7, 8)) --g(x)--> List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = ???
//      foldLeft(l, List[B]())((b, a) => f(a))

    // EXERCISE 21: Can you use flatMap to implement filter?

    // EXERCISE 22: Write a function that accepts two lists and constructs a new list
    // by adding corresponding elements. For example, List(1,2,3) and List(4,5,6) becomes List(5,7,9)

    // EXERCISE 23: Generalize the function you just wrote so that it's not specific to integers or addition.

    // EXERCISE 24 (hard): As an example, implement hasSubsequence for checking whether
    // a List contains another List as a subsequence

    //
    // EXERCISE 25: Write a function size that counts the number of nodes in a tree.
    // EXERCISE 26: Write a function maximum that returns the maximum element in a Tree[Int].
    // (Note: in Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)

    // EXERCISE 27: Write a function depth that returns the maximum path length
    // from the root of a tree to any leaf.

    // EXERCISE 28: Write a function map, analogous to the method of the same name on List, that
    // modifies each element in a tree with a given function

    // EXERCISE 29: Generalize size, maximum, depth, and map, writing a new function fold that abstracts
    // over their similarities. Reimplement them in terms of this more general function.
    // Can you draw an analogy between this fold function and the left and right folds for List?
  }
}