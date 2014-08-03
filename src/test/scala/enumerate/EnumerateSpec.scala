package enumerate

import org.scalatest._
import srcmain._
import srctest._

class EnumerateSpec extends FlatSpec with Matchers {

  behavior of "#withName"

  it should "work with a sealed trait in /src/main" in {
    withName[Foo]("Bar") should be(Some(Bar))
  }

  it should "work with a sealed trait in /src/test" in {
    withName[Goo]("Car") should be(Some(Car))
  }

}
