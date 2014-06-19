package luno.fpinscala

object Ch02 {

  // Exercise 1: Write a function to compute the nth fibonacci number
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, p0: Int, p1: Int):Int = {
      if(n == 1) p1
      else go(n - 1, p1, p0 + p1)
    }

    if(n == 0) 0
    else go(n, 0, 1)
  }

  // Exercise 2: Implement a polymorphic function to check whether an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int): Boolean = {
      if(i + 1 >= as.length) true
      else if(!gt(as(i), as(i + 1))) go(i + 1)
      else false
    }
    go(0)
  }

  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation! Here's an example:

  // Exercise 3: Implement `partial1`.
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)


  // Exercise 4: Implement `curry`.
  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  // NB: The `Function2` trait has a `curried` method already

  // Exercise 5: Implement `uncurry`
  // Note that since => associates to the right, A => (B => C) can be written as A => B => C.
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  /*
  NB: There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.

  Note that we can go back and forth between the two forms. We can curry
  and uncurry and the two forms are in some sense "the same". In FP jargon,
  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
  a term we inherit from category theory.
  */

  // Exercise 6: Implement `compose`
  // You need to return a new function of type `A => C`. Start by accepting an argument of type `A`. Now follow the types. You have an `A`. What can you do with it? Do you have a function that accepts an `A`?
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
 }
