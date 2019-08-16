package ecosim.classLifting

import ecosim.deep.IR
import IR.Predef._


//contains everything except the body of the info
//usage - holds all of the information relevant for other methods, but can be used safely before collecting data for
//all existing methods (which would not be possible with the body)
class MethodInfo[A: CodeType](
                              val symbol: IR.MtdSymbol,
                              val tparams: List[IR.TypParam],
                              val vparams: List[List[IR.Variable[_]]],
                              val blocking: Boolean)

