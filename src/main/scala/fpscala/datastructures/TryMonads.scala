package fpscala.datastructures

import scala.util.{Failure, Success, Try}

/**
 * Created by stefan on 30/10/15.
 */
object TryMonads extends App {

  def f[T](arg: T): Try[T] = arg match {
    case 5 => Failure(new IllegalArgumentException("bad value"))
    case x @ _ => Success(x)
  }

  def g(s: String) = {
    val result = for {
      step1 <- f(s)
      step2 <- f(step1.length)
      step3 <- f("_" * step2)
    } yield step3

    println(s""""$s" => $result""")
  }

  g("how are you")
  g("hello")

}
