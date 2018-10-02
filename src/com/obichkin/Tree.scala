package com.obichkin

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def map2[A,B](t: Tree[A])(f: A=>B): Tree[B] = {
    fold(t)( a=>Leaf(f(a)):Tree[B] )(Branch(_,_))
  }

  def max2(t: Tree[Int]): Int = fold(t)(a=>a)((b1,b2)=>b1 max b2)

  def size2[A](t: Tree[A]): Int = fold(t)((a)=>1)(_+_+1)

  def depth2[A](t: Tree[A]): Int = fold(t)( a=>1 )( (b1,b2)=>(b1 max b2)+1 )

  def fold[A,B](t: Tree[A])(f: A=>B)(g: (B,B)=>B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g( fold(l)(f)(g), fold(r)(f)(g)  )
  }

  def map0[A,B](t: Tree[A])(f: A=>B): Tree[B] = t match {
    case Leaf(i) => Leaf( f(i) )
    case Branch(l, r) => Branch( map0(l)(f), map0(r)(f))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => (1+depth(l)) max (1+depth(r))
  }

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l, r) => max(l) max max(r)
  }

}
