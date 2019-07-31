package ecosim.runtime


import Simulation.{Message, RequestMessage, RequestMessage2, SimO}
import code.{Instruction, __do}
import ecosim.deep.Interpreter.Assignment
import ecosim.deep.{CallMethod, IR, Interpreter}


class Actor extends SimO {
  override def mycopy(): SimO = {
    new Actor()
  }
  private[ecosim] var owner: Simulation = _

  override def algo: Instruction = __do{}

  setMessageHandler("RequestMessage", (message: Message) => {
    val mCast:RequestMessage = message.asInstanceOf[RequestMessage]
    val result = mCast.call_f(this)
    mCast.reply(this, result)
  })

  setMessageHandler("RequestMessage2", (message: Message) => {
    val mCast:RequestMessage2 = message.asInstanceOf[RequestMessage2]

    //TODO: call method
    /*val argtypes = mCast.args
    val args = mCast.args.map(_.asInstanceOf[AnyRef])
    val methods = this.getClass.getMethods

    for (method <- methods) {
      if (method.getName == mCast.methodName) {
        var parameterTypes = method.getParameterTypes
        println("P", parameterTypes.mkString(","))
        println("A", argtypes.mkString(","))
        if (parameterTypes.length == argtypes.length) {
          parameterTypes = parameterTypes.map{x => x match {
            case java.lang.Boolean.TYPE => classOf[java.lang.Boolean]
            case java.lang.Character.TYPE => classOf[java.lang.Character]
            case java.lang.Byte.TYPE => classOf[java.lang.Byte]
            case java.lang.Short.TYPE => classOf[java.lang.Short]
            case java.lang.Integer.TYPE => classOf[java.lang.Integer]
            case java.lang.Long.TYPE => classOf[ java.lang.Long]
            case java.lang.Float.TYPE => classOf[java.lang.Float]
            case java.lang.Double.TYPE => classOf[java.lang.Double]
            case java.lang.Void.TYPE => classOf[java.lang.Void]
            case x => x
          }}

          parameterTypes.foreach(println)
          if (argtypes.zip(parameterTypes).forall(x => x._1 == x._2)) {
            println("Method found")
            method.invoke(this, args:_*)
          }
        }
      }
    }*/

  })

}
class Simulation {
  
}
