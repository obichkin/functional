package com.obichkin

import com.obichkin.List._
import com.obichkin.Tree._
import scala.annotation.tailrec


object Main extends App {

  val g = List(1,2,3,4,5)
  val h = List(6,7,8)
  val i = List(9,10)


  val t0 = Leaf(3)
  val t1 = Leaf(7)
  val t2 = Leaf(4)
  val tk = Branch(t0, t2)
  val td = Branch(t1, tk)
  val tt = Branch(t0, td)

  def ff = (i:Int)=>i*2

  println(map0(td)(ff))
  println(map2(td)(ff))



  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def fac(i:Int): Long = {
    i match {
      case 0 => 1
      case x: Int => (1 to x).foldLeft[Long](1)( (y:Long, x:Int)=>x.toLong*y )
    }
  }


  def factorial(n: Long): Long = {
      @tailrec
      def loop(f: Long, n:Long): Long = {
        n match {
          case 0 => f
          case _ => loop(f*n, n-1)
        }
      }
      loop(1, n)
    }

  def greaterThan = curry( (x:Int, y:Int) => x > y )

  def thanGreater = uncurry((x:Int)=> (y:Int) => x>y )

  def absGreater5 = compose( (b:Int) => b > 5   , (a:Int) => a.abs )

  def curry[A,B,C]( f: (A,B)=>C ): A=>(B=>C) = {
    (a:A) => (b:B) => f(a,b)
  }

  def uncurry[A,B,C](f: A=>B=>C): (A,B)=>C = {
    (a:A, b:B) => f(a)(b)
  }

  def compose[A,B,C](f:B=>C, g:A=>B): A=>C = {
    f compose g
  }

  def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def loop(n: Int): Boolean ={
      if( n >= as.length-1 ) true
      else if( ordered(as(n), as(n+1)) ){ loop(n+1) }
      else false
    }

    loop(0)
  }


  def fib2(n: Int): Long = {
    var x = 0
    var x_2 = 0
    var x_1 = 1

    for(i <- 2 to n){
      x = x_2 + x_1
      x_2 = x_1
      x_1 = x
    }

    x
  }

  @tailrec
  def fib3(n: Int, prev: Int = 0, cur: Int = 1): Long = {
    n match {
      case 0 => prev
      case 1 => cur
      case _ => fib3(n-1, cur, prev+cur)
    }

  }
}
