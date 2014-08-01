package enumerate

import org.scalatest._
import models._

class EnumerateSpec extends FlatSpec with Matchers {

  behavior of "#withName"

  it should "find the object with the given name" in {
    val result: Option[Foo] = withName[Foo]("Ba" + "r")
    result should be(Some(Bar))
  }

  it should "return None if no object with the given name exists" in {
    val result: Option[Foo] = withName[Foo]("OhMyWord")
    result should be(None)
  }


}
