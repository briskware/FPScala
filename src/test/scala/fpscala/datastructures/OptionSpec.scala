package fpscala.datastructures

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.verb._

import scala.languageFeature.postfixOps

import org.scalatest.FlatSpec

//import fpscala.datastructures.Option._

class OptionSpec extends FlatSpec with Matchers {

  lazy val failMe = assert(false)

  "An Option" should "have an apply method" in {
    val x = Som(1)
    assert(x.get === 1)
  }

  "An Option" should "have map and getOrElse" in {
    val x = Som(1)
    val y = x map ( _ * 5 )

    assert(y.getOrElse(failMe) === 5)
    assert(Non.getOrElse(1) === 1)
  }

  "An Option" should "have flatMap" in {
    val x = Som(1)
    val y = x flatMap ( i => Non )
    assert(y === Non)
    val z = x flatMap ( i => Som(i) )
    assert(z.getOrElse(failMe) === 1)
  }

}
