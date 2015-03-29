package fpscala.datastructures

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.verb._

import scala.languageFeature.postfixOps

import org.scalatest.FlatSpec

import fpscala.datastructures.List._

/**
 * Created by stefan on 27/09/2014.
 */
class ListSpec extends FlatSpec with Matchers {


  "A List" should "have an apply method" in {
    val x = List(1,2,3)
    x != Nil
  }

  "A List" should "also be empty" in {
    assert(List() === Nil)
  }

  "A List" should "have a head and tail" in {
    val x = List(1,2,3,4)
    println(x)
    println(x.reverse)
    assert(x.head === 1)
    assert(x.tail === List(2,3,4))
  }

  "A Nil List" should "throw on accessing head" in {
    intercept[NoSuchElementException] {
      Nil.head
    }
  }

  "A Nil List" should "throw on accessing tail" in {
    intercept[NoSuchElementException] {
      Nil.tail
    }
  }

  "toString" should "print list members correctly" in {
    assert(s"List(1,2,3)" === List(1,2,3).toString)
    assert(s"List(3,2,1)" === List(1,2,3).reverse.toString)
  }

  "foldLeft and foldRight" should "do the right thing" in {
    val x = List(1,2,3,4,5,6,7,8,9)
    assert(45 === x.foldLeft(0)(_+_))
    assert(45 === x.foldRight(0)(_+_))
  }

  "foldLeft member" should "do the right thing" in {
    assert(45 === List(1,2,3,4,5,6,7,8,9).foldLeft(0)(_+_))
  }

  "lenght" should "return the correct value" in {
    assert(3 === List(1,2,3).length)
  }

  "append" should "correctly prepend an element" in {
    val x = 1 :: List(2,3)
    println(x)
    assert(List(1,2,3) === x)
  }

  "concatenate" should "should join lists" in {
    val x = List(1,2,3) ++ List(7,8,9)
    assert(List(1,2,3,7,8,9) === x)
  }

  "map" should "transform the values" in {
    val x = List(1,2,3) map {
      x => x + x
    }
    assert(List(2,4,6) === x)
  }

  "flatMap" should "transform each element to a list and then flatten the result" in {
    val x = List(1,2,3) flatMap {
      x => List(x,x+x,x+x+x)
    }
    assert(List(1,2,3,2,4,6,3,6,9) === x)
  }

  "filter" should "remove values that do not match the passed function" in {
    val x = List(1,9,2,8,3,7,4,6,5) filter {
      _ < 5
    }
    assert(List(1,2,3,4) === x)
  }

  "zip" should "merge the lists to pairs" in {
    val x = List(1,2,3) zip List(2,4,6)
    assert(List((1,2),(2,4),(3,6)) === x)
  }

  "zipWith" should "merge the list calculating the value" in {
    val x = List(1, 2, 4).zipWith(List(2,4,6)) {
      (a,b) => a * b
    }
    println(x)
    assert(List(2,8,24) === x)
  }

  "zipWithIndex" should "index the elements of the list" in {
    val x = List("Scala","programming","is","awesome").zipWithIndex
    println(x)
    assert(List(("Scala",0),("programming",1),("is",2),("awesome",3)) === x)
  }

  "take" should "returns the first n values from the list" in {
    val x = List(1,2,3,4,5,6,7,8,9,10).take(5)
    assert(List(1,2,3,4) === x)
  }

  ignore should "be tail recursive" in {
    intercept[StackOverflowError] {
      val x: List[Int] = (1 to 200000).foldLeft(List[Int]()) {
        (acc: List[Int], y: Int) => Cons(y, acc)
      }
      val y: Int = x.foldLeft(0) {
        (i: Int, v: Int) => v+1
      }
      println(x)
    }
  }

  "last" should "return the last value of the list" in {
    assert(List(1,2,3).last === 3)
    assert(List(1).last === 1)
  }


}
