package enumerate

import org.scalatest._
import models._

class EnumerateSpec extends FlatSpec with Matchers {

  behavior of "#withName"

  it should "find the object with the given name" in {
    withName[Foo]("Ba" + "r") should be(Some(Bar))
  }

  it should "return None if no object with the given name exists" in {
    withName[Foo]("OhMyWord") should be(None)
  }

  it should "work with a sealed abstract class" in {
    withName[Wow]("Yolo") should be(Some(Yolo))
  }

  it should "not find a case class with the given name" in {
    withName[Foo]("WoahNow") should be(None)
  }

  it should "throw a compile error if A is not a sealed trait/abstract class" in {
    """withName[Bar$]("Bar")""" shouldNot compile
  }


}
