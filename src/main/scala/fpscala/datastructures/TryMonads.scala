package fpscala.datastructures

import scala.util.{Success, Try}

/**
 * Created by stefan on 30/10/15.
 */
object TryMonads extends App {

  def f[T](arg: T): Try[T] = {
    Success(arg)
  }

  var result = for {
    step1 <- f("hello")
    step2 <- f(step1.length)
    step3 <- f("_" * step2)
  } yield step3

  println(s"result=$result")
}
