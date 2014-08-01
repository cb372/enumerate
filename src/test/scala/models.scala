package models 

sealed trait Foo
case object Bar extends Foo
case object Baz extends Foo
case class WoahNow(a: Int) extends Foo

sealed abstract class Wow
case object Yeah extends Wow
case object Yolo extends Wow

