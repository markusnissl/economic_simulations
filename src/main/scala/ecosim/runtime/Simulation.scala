package ecosim.runtime


import Simulation.{Message, RequestMessage, RequestMessage2, SimO}
import code.{Instruction, __do}

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
    /*println(mCast.args)
    println(classOf[Int])
    val argtypes = mCast.args.map(x => {
      if (x.isInstanceOf[AnyRef]) x.getClass
      else classOf[Int]
    })
    println(argtypes)
    val args = mCast.args.map(_.asInstanceOf[AnyRef])
    val method = this.getClass.getMethod(mCast.methodName, argtypes:_*)
    method.invoke(this, args)*/
  })

}
class Simulation {
  
}
