package fpscala.datastructures

import java.util.NoSuchElementException

import scala.annotation.tailrec


sealed trait List[+A] {
  def head: A
  def tail: List[A]

  def foldLeft[B](z: B)(f: (A,B) => B): B = {
    @tailrec
    def loop(as: List[A],z: B): B = as match {
      case Nil => z
      case Cons(x, xs) => loop(xs,f(x,z))
    }
    loop(this, z)
  }

  def foldRight[B](z: B)(f: (A,B) => B): B = reverse.foldLeft(z)(f)

  def last: A = {
    @tailrec
    def loop(as: List[A]): A = as match {
      case Cons(t, Nil) => t
      case Cons(x, xs) => loop(xs)
    }
    loop(this)
  }

  def length: Int = foldLeft(0)((a,b) => b + 1)

  def ::[B >: A](b: B): List[B] = List(b)
  def ++[B >: A](b: List[B]): List[B] = b

  override def toString(): String = {
    val s = foldLeft("")( (l, s) => if (s.length == 0) s"$l" else s"$s,$l")
    s"List(${s})"
  }

  def reverse: List[A] = {
    @tailrec
    def loop(l: List[A], r: List[A]): List[A] = l match {
      case Nil => r
      case Cons(x, xs) => loop(xs, Cons(x, r))
    }
    loop(this, Nil)
  }

  def map[B](f: A => B): List[B] = foldRight(List[B]()) {
    (x: A,l: List[B]) => f(x) :: l
  }

  def flatMap[B](f: A => List[B]): List[B] = foldRight(List[B]()) {
    (x: A, l: List[B]) => f(x) ++ l
  }

  def filter(f: A => Boolean): List[A] = foldRight(List[A]()) {
    (x: A, l: List[A]) => if ( f(x) ) x :: l else l
  }

  def zip[B >: A](b: List[B]): List[(A,B)] = zipWith(b)((_,_))

  def zipWith[B >: A, C](b: List[B])(f: (A,B) => C): List[C] = {
    def loop(as: List[A], bs: List[B]): List[C] = (as, bs) match {
      case (Cons(x, xs), Cons(y, ys)) => f(x,y) :: loop(xs, ys)
      case (_,_) => Nil
    }
    loop(this,b)
  }

  def zipWithIndex: List[(A,Int)] = (foldLeft((List[(A,Int)](),0)) {
    (x: A, ls: (List[(A,Int)],Int)) => ((x,ls._2) :: ls._1,ls._2 + 1)
  }._1).reverse

  def take(n: Int): List[A] = {
    def loop(as: List[A], n: Int, rs: List[A]): List[A] = as match {
      case Nil => rs
      case Cons(x, xs) => if (n-1 > 0) loop(xs, n-1, rs ++ List(x)) else rs
    }
    loop(this, n, Nil)
  }

  def take2(n: Int): List[A] = this.zipWithIndex.flatMap {
    x => if (x._2 < n - 1) List(x._1) else Nil
  }

}

case object Nil extends List[Nothing] {
  override def head: Nothing = throw new NoSuchElementException("head of empty list")
  override def tail: List[Nothing] =  throw new NoSuchElementException("tail of empty list")

}

case class Cons[+A](_h: A, _t: List[A]) extends List[A] {
  override def head = _h
  override def tail = _t
  override def ::[B >: A](b: B): List[B] = Cons(b, this)
  override def ++[B >: A](b: List[B]): List[B] = foldRight(b)((e: A, l: List[B]) => e :: l)
}

object List {
  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
}
