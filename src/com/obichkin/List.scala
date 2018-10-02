package com.obichkin


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def hasSubSequence[A](l1: List[A], l2: List[A]): Boolean = {
    def sub(l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
      case ( Cons(h1,t1), Cons(h2,t2) ) => if (h1==h2) sub(t1, t2) else false
      case (Nil, Nil) => false
      case _ => true
    }

    l1 match {
      case Cons(h1, t1) => sub(l1, l2) || sub(t1, l2)
      case _ => false
    }
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B)=>C): List[C] = (l1, l2) match {
    case ( Cons(h1,t1), Cons(h2,t2) ) => Cons( f(h1, h2), zipWith(t1,t2)(f))
    case _ => Nil
  }

  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case ( Cons(h1, t1), Cons(h2, t2) ) => Cons(h1+h2, zipAdd(t1,t2))
    case _ => Nil
  }

  def filter2[A](l: List[A])(f: A=>Boolean): List[A] = {
    flatMap(l)(a=> if (f(a)) List(a) else Nil)
  }

  def flatMap[A,B](l: List[A])(f: A=>List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def filter[A](l: List[A])(f: A=>Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  def map[A,B](l: List[A])(f: A=>B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons( h.toString, doubleToString(t) )
  }

  def add1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h+1, add1(t))
  }

  def flatMap[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((l, r)=> append(l, r))

  def append2[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_,_))

  def append3[A](l1: List[A], l2: List[A]): List[A] = {
    foldLeft(l1, l2)((l, r) => append(l, List(r)) )
  }

  def reverse3[A](l: List[A]): List[A] = foldLeft(Nil:List[A], l)((left,right) => Cons(right,left))

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append( reverse(t), List(h))
  }

  def length3[A](l: List[A]) = foldLeft(0, l)((n,_) => n+1)

  def product3(l: List[Int]) = foldLeft(1, l)(_*_)

  def sum3(l: List[Int]) = foldLeft(0,l)(_+_)

  def foldLeft[A,B](b: B, l: List[A])(f: (B,A)=>B): B = l match {
    case Nil => b
    case Cons(h, t) => foldLeft(f(b, h), t)(f)
  }

  def length2[A](l: List[A]): Int = foldRight(l, 0)((_, n) => n+1)

  def sum2(l: List[Int]): Int = foldRight(l, 0)(_+_)

  def product2(l: List[Int]): Int = foldRight(l, 1)(_*_)

  def foldRight[A, B](l: List[A], b: B)(f: (A, B)=> B): B = l match {
    case Nil => b
    case Cons(h, t) => f(h, foldRight(t, b)(f))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def drop[A](l: List[A], n: Int): List[A] = if(n==0) l else drop(tail(l), n-1)

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  def head(l: List[Int]): Int = l match {
    case Cons(h, t) => h
    case Nil => 0
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(h, t) => if (h==0) 0 else h*product(t)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

}
