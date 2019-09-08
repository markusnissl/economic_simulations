package ecosim.deep.algo

import ecosim.deep.IR.Predef._
import ecosim.deep.member.ResponseMessage
import squid.lib.MutVar

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * This object contains shared data between the algo codegens
  * so that it can be accessed from every algo subclass
  */
object AlgoInfo {

  //This resets the variables which have to be unique per actorType
  def resetData(): Unit = {
    this.stateGraph.clear()
    this.variables = List()
    this.varSavers = List()
    this.posCounter = 0
    this.positionStack = Variable[ListBuffer[Int]]
    this.returnValue = Variable[MutVar[Any]]
    this.responseMessage = Variable[MutVar[ResponseMessage]]
  }

  /**
    * Helper class to save mappings between a variable introduced to a mutable variable, which is used in
    * the program. This is necessary, since it is not possible to define var variable references in squid
    * at the moment, thus we have to use the MutVar Wrapper to change the variable afterwards
    *
    * @param from original variable defined by Algo
    * @param to   new mutation variable created as replacement for original one
    * @param A    code type of original variable
    * @tparam C type of original variable
    */

  case class VarWrapper[C](from: Variable[C], to: Variable[MutVar[C]])(implicit val A: CodeType[C])
  /**
    * Variable containing the current execution time.
    * Need to be increased, if some waiting has to be done after
    * finishing the current step
    */
  val timeVar: Variable[MutVar[Int]] = Variable[MutVar[Int]]

  /**
    * Variable containing the current position of the program.
    * It needs to be modified if the next step is not the next
    * program fragment, but a jump to somewhere else
    */
  val positionVar: Variable[MutVar[Int]] = Variable[MutVar[Int]]

  /**
    * This stack is used to save a position into it, so that it can be used to jump
    * to it in a later part. For example, to jump back after a method call
    */
  var positionStack: Variable[ListBuffer[Int]] = Variable[ListBuffer[Int]]

  /**
    * This variables is used as a register to store the return value of a call.
    */
  var returnValue: Variable[MutVar[Any]] = Variable[MutVar[Any]]

  /**
    * This variable is used to save data about the response message, if a blocking call is made
    */
  var responseMessage: Variable[MutVar[ResponseMessage]] = Variable[MutVar[ResponseMessage]]

  /**
    * List, saving all variables, which should be defined at the beginning
    */
  var variables: List[VarWrapper[_]] = List()
  /**
    * List, saving all variables already defined, so that a redefinement of the variable
    * is not necessary inside the used fragment.
    */
  var varSavers: List[VarWrapper[_]] = List[VarWrapper[_]]()

  /**
    * Helper, for jumping relativly to a different position
    *
    * @param offset relative jumping offset
    * @return Code containing the position modification
    */
  def jump(offset: Int) = code"$positionVar := ($positionVar!) + ${Const(offset)}"

  /**
    * Current position/code fragment
    */
  var posCounter = 0

  /**
    * Go to next code fragment
    */
  def nextPos() {
    posCounter += 1
  }

  /**
    * Wrapper class for modeling a node id, which can ether be a method id or a position
    */
  abstract class CodeNode {
    def getId: String
    def getNativeId: Int
  }

  /**
    * Node for a position in code
    *
    * @param pos id of posCounter
    */
  case class CodeNodePos(pos: Int) extends CodeNode {
    override def toString: String = {
      pos.toString
    }

    override def getId: String = (pos.toString)
    override def getNativeId: Int = pos
  }

  /**
    * method id, which should reference the node to
    *
    * @param id  methodId, which node should be referenced
    * @param end if node is start or end of method
    */
  case class CodeNodeMtd(id: Int, end: Boolean = false) extends CodeNode {
    override def getId: String = "M" + id + (if (end) "E" else "")
    override def getNativeId: Int = id
  }

  /**
    * IF inside method, set to true, so that graph knows about that (just for displaying in a different color atm)
    */
  var isMethod = false
  var methodId = -1

  /**
    * Models an edge between to nodes
    *
    * @param label    a random name, which is displayed when drawing the graph
    * @param from     start node
    * @param to       end node
    * @param code     actual code, which is executed when
    * @param waitEdge an information, that this edge is increasing the timer
    * @param isMethod is filled out automatically by using the isMethod vaiable of this class
    * @param sendInfo if this edge is a send, keeps a reference to [[Send]] for relevant info, and also a
    *                 boolean which is true if this is the first in sequence of edges representing the send
    * @param methodId1 if the edge is part of method, keeps its id
    */
  //TODO change the structure so it contains any algo info instead of sendInfo, methodId, callMethodInfo, waitEdge and so on
  case class EdgeInfo(var label: String,
                      var from: CodeNode,
                      var to: CodeNode,
                      var code: OpenCode[Unit],
                      waitEdge: Boolean = false,
                      isMethod: Boolean = isMethod,
                      cond: OpenCode[Boolean] = null,
                      var storePosRef: List[List[EdgeInfo]] = Nil,
                      var sendInfo: (Send[_], Boolean) = null,
                      var methodId1: Int = methodId,
                      var methodCallInfo: CallMethod[_] = null) {

    def myCopy(): EdgeInfo = {
      EdgeInfo(label, from, to, code, waitEdge, isMethod, cond, storePosRef, sendInfo, methodId1, methodCallInfo)
    }

    def convertToPosOnly (methodLookupTable: Map[Int, Int], methodLookupTableEnd: Map[Int, Int]): Unit = {
      from match {
        case CodeNodeMtd(methodId, end) => {
          if (!end) {
            from = CodeNodePos(methodLookupTable(methodId))
          } else {
            from = CodeNodePos(methodLookupTableEnd(methodId))
          }
        }
        case CodeNodePos(_) => {}
      }

      to match {
        case CodeNodeMtd(methodId, end) => {
          if (!end) {
            to = CodeNodePos(methodLookupTable(methodId))
          } else {
            to = CodeNodePos(methodLookupTableEnd(methodId))
          }
        }
        case CodeNodePos(_) => {}
      }
    }
  }

  case class MergeInfo(from: CodeNodePos, to: CodeNodePos, graph1: (CodeNodePos, CodeNodePos), graph2: (CodeNodePos, CodeNodePos))

  /**
    * Stores the edges to build up a state transition graph
    */
  var stateGraph: ArrayBuffer[EdgeInfo] = ArrayBuffer[EdgeInfo]()

  def convertStageGraph(methodLookupTable: Map[Int, Int], methodLookupTableEnd: Map[Int, Int]) {
    AlgoInfo.stateGraph.foreach(_.convertToPosOnly(methodLookupTable, methodLookupTableEnd))
  }
}

