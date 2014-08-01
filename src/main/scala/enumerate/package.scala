import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object enumerate {
  private val errorMsg = "Sorry, you can only use withName with sealed traits and sealed abstract classes"

  /**
   * Look up a child object of a sealed trait/abstract class by name,
   * just like you can do with Enumeration#withName.
   */
  def withName[A](name: String): Option[A] = macro withNameImpl[A]

  def withNameImpl[A: c.WeakTypeTag](c: Context)(name: c.Tree) = {
    import c.universe._
    val typeSmb = weakTypeTag[A].tpe.typeSymbol

    // sanity checks
    if (!typeSmb.isClass) error(errorMsg)
    val classSmb = typeSmb.asClass
    if (!classSmb.isAbstract || !classSmb.isSealed) error(errorMsg)

    val subclassSymbols: Set[Symbol] = classSmb.knownDirectSubclasses
    // TODO recursively search for transitive subclasses

    // Map from symbol name to the corresponding class
    val symbolsMap: Map[String, Tree] = 
      subclassSymbols.map(s => (s.name.toString -> Ident(s.asClass.companionSymbol))).toMap

    // Just do a lookup in the symbols map
    q"${symbolsMap}.get($name)"
  }
}
