package ecosim

import scala.annotation.compileTimeOnly

package object deep {

  import IR.Predef._
  import squid.lib.MutVar

  @compileTimeOnly("This function can only be calld at compile time")
  def getMethodPosition(methodId: Int): Int = {
    ???
  }

  @compileTimeOnly("This function can only be calld at compile time")
  def setMethodParam(methodId: Int, argPos: Int, arg:Any): Unit = {
    ???
  }

  @compileTimeOnly("This function can only be calld at compile time")
  def saveMethodParam(methodId: Int, argPos: Int, arg:Any): Unit = {
    ???
  }

  @compileTimeOnly("This function can only be calld at compile time")
  def restoreMethodParams(methodId: Int): Unit = {
    ???
  }

}
