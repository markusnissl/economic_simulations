package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._

case class CallMethod[R: CodeType](methodId: Int, argss: List[List[OpenCode[_]]]) extends Algo[R] {

  override def codegen: List[IR.Predef.OpenCode[Unit]] = {
    val initParam:OpenCode[Any] = code"()"

    val flattendArgs = argss.flatten
    val setMethodParamsList = flattendArgs.zipWithIndex.foldRight(initParam)((a,b) => code"Instructions.setMethodParam(${Const(methodId)}, ${Const(a._2)}, ${a._1}); $b")
    val saveMethodParamsList = flattendArgs.zipWithIndex.foldRight(initParam)((a,b) => code"Instructions.saveMethodParam(${Const(methodId)}, ${Const(a._2)}, ${a._1}); $b")
    val restoreMethodParam = code"Instructions.restoreMethodParams(${Const(methodId)})"

    val f1: OpenCode[Unit] =
      code"""${AlgoInfo.positionStack}.prepend((${AlgoInfo.positionVar}!) + 1);
                 $saveMethodParamsList;
                 $setMethodParamsList;

                 ${AlgoInfo.positionVar} := (Instructions.getMethodPosition(${Const(methodId)}) - 1);
                 ()
            """
    // 3. Method will return to position pushed on stack and contain returnValue
    // 4. Restore save variable from stack
    val f2: OpenCode[Unit] =
    code"""
               $restoreMethodParam
          ()
          """
    AlgoInfo.merger.append((true, false))
    AlgoInfo.merger.append((true, true))
    List(f1, f2)
  }
}
