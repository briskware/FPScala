package fpscala.datastructures

sealed trait Opt[+A] {
  def map[B](f: A => B): Opt[B] = this match {
    case Non => Non
    case Som(a) => Som(f(a))
  }
  def flatMap[B](f: A=> Opt[B]): Opt[B] = this match {
    case Non => Non
    case Som(a) => f(a)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Non => default
    case Som(a) => a
  }
  def orElse[B >: A](ob: => Opt[B]): Opt[B] = this match {
    case Non => ob
    case Som(a) => this
  }
  def filter(f: A => Boolean): Opt[A] = this match {
    case Non => Non
    case Som(a) => if (f(a)) Som(a) else Non
  }

}

case class Som[+A](get: A) extends Opt[A]
case object Non extends Opt[Nothing]
