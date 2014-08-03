import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object enumerate {
  private def errorMsg(methodName: String) = s"Sorry, you can only use ${methodName} with sealed traits and sealed abstract classes"

  /**
   * Look up a child case object of a sealed trait/abstract class by name,
   * just like you can do with Enumeration#withName.
   */
  def withName[A](name: String): Option[A] = macro withNameImpl[A]

  def withNameImpl[A: c.WeakTypeTag](c: Context)(name: c.Tree) = {
    import c.universe._

    val subclassSymbols: Set[Symbol] = symbols(c, "withName")
    
    // Map from symbol name to the corresponding class
    val symbolsMap: Map[String, Tree] = 
      subclassSymbols.map(s => (s.name.toString -> Ident(s.asClass.companionSymbol))).toMap

    // Just do a lookup in the symbols map
    q"${symbolsMap}.get($name)"
  }

  private def symbols[A: c.WeakTypeTag](c: Context, methodName: String): Set[c.universe.Symbol] = {
    import c.universe._

    val typeSmb = weakTypeTag[A].tpe.typeSymbol

    // sanity checks
    if (!typeSmb.isClass) error(errorMsg(methodName))
    val classSmb = typeSmb.asClass
    if (!classSmb.isAbstract || !classSmb.isSealed) error(errorMsg(methodName))

    // Find all direct child case objects (filter out everything else)
    classSmb.knownDirectSubclasses
      .filter(smb => smb.isClass && /*smb.asClass.isCaseClass &&*/ smb.asClass.isModuleClass) 
      // Weird! Sometimes the compiler thinks a given case object is a case class, sometimes it changes its mind
  }

  /**
   * Enamurate all child case objects of a sealed trait/abstract class.
   * just like you can do with Enumeration#values.
   */
  def values[A]: Set[A] = macro valuesImpl[A]

  def valuesImpl[A: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val subclassSymbols: Set[Symbol] = symbols(c, "values")

    val instances: Set[Tree] = subclassSymbols.map(s => Ident(s.asClass.companionSymbol))

    q"${instances}"
  }

}
