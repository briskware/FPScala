package fpscala

import scala.util.{Failure, Success, Try}


/**
 * Created by stefan on 30/10/15.
 */
object TryMonads extends App {

  def f[T](arg: T): Try[T] = arg match {
    case 5 => Failure(new IllegalArgumentException("bad value"))
    case x @ _ => Success(x)
  }

  def o[T](arg: T): Option[T] = arg match {
    case 5 => None
    case x @ _ => Some(x)
  }

  def g(ss: Seq[String]) = {
    for {
      s <- ss
      step1 <- f(s)
      step2 <- f(step1.length)
      step3 <- f("_" * step2)
    } yield step3
  }

  println(g("how are you" :: "hello" :: Nil))

}
