import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object enumerate {
  private val errorMsg = "Sorry, you can only use withName with sealed traits and sealed abstract classes"

  /**
   * Look up a child case object of a sealed trait/abstract class by name,
   * just like you can do with Enumeration#withName.
   */
  def withName[A](name: String): Option[A] = macro withNameImpl[A]

  def withNameImpl[A: c.WeakTypeTag](c: Context)(name: c.Tree) = {
    import c.universe._
    val typeSmb = weakTypeTag[A].tpe.typeSymbol

    // sanity checks
    if (!typeSmb.isClass) c.error(c.enclosingPosition, errorMsg)
    val classSmb = typeSmb.asClass
    if (!classSmb.isAbstract || !classSmb.isSealed) c.error(c.enclosingPosition, errorMsg)

    println(s"Known direct subclasses of $classSmb : ${classSmb.knownDirectSubclasses}")

    // Find all direct child case objects (filter out everything else)
    val subclassSymbols: Set[Symbol] = classSmb.knownDirectSubclasses
      .filter(smb => smb.isClass && /*smb.asClass.isCaseClass &&*/ smb.asClass.isModuleClass) 
      // Weird! Sometimes the compiler thinks a given case object is a case class, sometimes it changes its mind
    
    // Map from symbol name to the corresponding class
    val symbolsMap: Map[String, Tree] = 
      subclassSymbols.map(s => (s.name.toString -> Ident(s.asClass.companionSymbol))).toMap

    // Just do a lookup in the symbols map
    q"${symbolsMap}.get($name)"
  }
}
