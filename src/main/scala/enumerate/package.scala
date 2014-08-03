import scala.language.experimental.macros

package object enumerate {

  /**
   * Look up a child case object of a sealed trait/abstract class by name,
   * just like you can do with Enumeration#withName.
   */
  def withName[A](name: String): Option[A] = macro Macros.withNameImpl[A]
 
  /**
   * Enumerate all child case objects of a sealed trait/abstract class,
   * just like you can do with Enumeration#values.
   */
  def values[A]: Set[A] = macro Macros.valuesImpl[A]

}
