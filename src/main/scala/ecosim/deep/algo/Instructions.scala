package ecosim.deep.algo

import scala.annotation.compileTimeOnly

object Instructions {

  /**
    * Static resolution of position of methodId after generating the code and the positions are fixed
    * @param methodId id of method you want to call
    * @return position in code
    */
  @compileTimeOnly("This function can only be called at compile time")
  def getMethodPosition(methodId: Int): Int = {
    ???
  }

  /**
    * sets the register of the method with the given arguement
    * @param methodId id of method you call
    * @param argPos position of argument (Starting with 0, if multiple lists, just continue counting)
    * @param arg value of argument
    */
  @compileTimeOnly("This function can only be called at compile time")
  def setMethodParam(methodId: Int, argPos: Int, arg:Any): Unit = {
    ???
  }

  /**
    * Puhsed the arg on the stack for the given method and argument position
    * @param methodId id of method you want to put the argument on the stack
    * @param argPos position of argument (Starting with 0, if multiple lists, just continue counting)
    * @param arg value of argument
    */
  @compileTimeOnly("This function can only be called at compile time")
  def saveMethodParam(methodId: Int, argPos: Int, arg:Any): Unit = {
    ???
  }

  /**
    * Restores saved method params of the given method id.
    * @param methodId id of method you want to restore the saved arguments
    */
  @compileTimeOnly("This function can only be called at compile time")
  def restoreMethodParams(methodId: Int): Unit = {
    ???
  }

  /**
    * Used as internal helper, to split class code by pattern matching.
    * PLEASE DO NOT USE WITHOUT KNOWING WHAT TO DO.
    * This code is not automatically replaced with other code.
    */
  def splitter(): Unit = {???}
}
