package calc

package object stdlib {

  def isLibFunction(fname: String): Boolean =
    Math.functionTypes.contains(fname)

  def libFunType(fname: String): TreeType =
    Math.functionTypes.getOrElse(fname, NoType)

}
