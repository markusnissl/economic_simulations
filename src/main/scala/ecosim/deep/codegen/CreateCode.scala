package ecosim.deep.codegen

import java.io.{BufferedWriter, File, FileWriter}

import ecosim.deep.IR
import ecosim.deep.algo.AlgoInfo
import ecosim.deep.algo.AlgoInfo.{CodeNodePos, EdgeInfo}

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import ecosim.deep.IR.Predef._
import ecosim.deep.member.Actor
import squid.lib.MutVar


class CreateCode(initCode: OpenCode[List[Actor]]) extends StateMachineElement() {

  var compiledActorGraphs: List[CompiledActorGraph] = Nil
  override def run(compiledActorGraphs: List[CompiledActorGraph]): List[CompiledActorGraph] = {
    this.compiledActorGraphs = compiledActorGraphs

    for (cAG <- this.compiledActorGraphs) {
      prepareClass(cAG)
    }

    createInit(IR.showScala(initCode.rep))

    null
  }


  def prepareClass(compiledActorGraph: CompiledActorGraph): Unit = {
    val selfs = compiledActorGraph.actorTypes.map(actorType => actorType.self.toCode.toString().substring(5).dropRight(1))

    val commands = generateCode(compiledActorGraph)
    val code = this.createCommandOpenCode(commands)
    val codeWithInit = this.generateVarInit(compiledActorGraph.variables2, this.generateMutVarInit(compiledActorGraph.variables,
      code"""
              val ${AlgoInfo.timeVar} = MutVar(0)
              val ${AlgoInfo.positionVar} = MutVar(0)
              val getCommands = () => $code
              val commands = getCommands()
              ecosim.deep.algo.Instructions.splitter
              (until: Int) => {
                while ((${AlgoInfo.timeVar}!) <= until && (${AlgoInfo.positionVar}!) < commands.length) {
                  val command = commands((${AlgoInfo.positionVar}!))
                  command()
                }
                ${compiledActorGraph.actorTypes.head.self}
              }
          """))

    val scalaCode = IR.showScala(codeWithInit.rep)
    var replacedTypes = scalaCode
    selfs.foreach(x => replacedTypes = replacedTypes.replace(x, "this"))
    val steps = changeTypes(replacedTypes)

    //Needed to split, so that a function can be extracted from the code, to write everything as class variables
    val parts = steps.split("""ecosim\.deep\.algo\.Instructions\.splitter\(\);""")
    val initVars = parts(0).substring(2)
    //This ugly syntax is needed to replace the received code with a correct function definition
    val run_until = "override def run_until" + parts(1).trim().substring(1).replaceFirst("=>", ": ecosim.deep.member.Actor = ").dropRight(1).trim.dropRight(1)

    // Converts all initParams to state variables again
    var initParams = ""
    for (actorType <- compiledActorGraph.actorTypes) {
      for (s <- actorType.states) {
        initParams = initParams + "var " + s.sym.name + ": " + changeTypes(s.tpe.rep.toString) + " = " + changeTypes(IR.showScala(s.init.rep)) + "\n"
      }
    }

    createClass(compiledActorGraph.name, initParams, initVars, run_until)
  }

  def generateCode(compiledActorGraph: CompiledActorGraph): List[OpenCode[Unit]] = {
    val graph: ArrayBuffer[EdgeInfo] = compiledActorGraph.graph
    //Reassign positions
    var positionMap: Map[Int, Int] = Map()

    val groupedGraph = graph.groupBy(_.from.getNativeId)

    var code: ArrayBuffer[OpenCode[Unit]] = ArrayBuffer[OpenCode[Unit]]()

    var changeCodePos: List[(Int, EdgeInfo)] = List()
    var requiredSavings: List[Int] = List()
    var posEdgeSaving: Map[(Int, Int), Int] = Map()

    def generateCodeInner(node: Int): Unit = {

      positionMap = positionMap + (node -> code.length)

      val start = groupedGraph.getOrElse(node, ArrayBuffer[EdgeInfo]())

      //If we have more than one unknown cond, we have to store the edges to the list, so that the position can be looked up
      var unknownCondNode: Int = 0
      start.foreach(edge => {
        if (edge.cond == null) {
          unknownCondNode = unknownCondNode + 1
        }
      })

      // Assume, a node has either only conditions or not any.
      // If in future something is different this line throws an error
      // to show that this thing has to be reimplemented to find a ways to know
      // whether a condition should be followed or a jump executed
      // At the moment conditions only apply in if statements, thus it has two
      // Outgoing edges and thus are not merged, thus this should not happen
      // Therefore we assume, if unknownCondNode == 0 then this is a conditional edge
      assert(start.length == unknownCondNode || unknownCondNode == 0)

      start.zipWithIndex.foreach(edgeIndex => {
        val edge = edgeIndex._1
        val target = edge.to.getNativeId

        //Go to next free pos, and if already next node code is defined go to first code fragment of next node
        val nextPos = positionMap.getOrElse(target, code.length + 1)

        //If there are more than one unknown cond, we have to get the position from stack
        var unknownCond: Int = 0
        groupedGraph.getOrElse(target, ArrayBuffer[EdgeInfo]()).foreach(edge2 => {
          if (edge2.cond == null) {
            unknownCond = unknownCond + 1
          }
        })
        if (unknownCond > 1) {
          requiredSavings = target :: requiredSavings
        }

        var posChanger: OpenCode[Unit] = code"${AlgoInfo.positionVar} := ${Const(nextPos)}"
        if (unknownCond > 1) {
          posChanger = code"${AlgoInfo.positionVar} := ${edge.positionStack}.remove(0).find(x => x._1 == (${Const(edge.edgeState._1)},${Const(edge.edgeState._2)})).get._2; ()"
        }

        val currentCodePos = code.length

        if (edge.storePosRef.nonEmpty) {
          changeCodePos = (currentCodePos, edge) :: changeCodePos
        }
        if (unknownCondNode > 1 && edge.cond == null) {
          posEdgeSaving = posEdgeSaving + ((node, target) -> currentCodePos)
        }

        if (edge.cond != null) {
          code.append(code"if(${edge.cond}) {${edge.code}; $posChanger} else {}")
        } else {
          code.append(code"${edge.code}; $posChanger")
        }

        if (nextPos == code.length) {
          generateCodeInner(target)
        }

        //Rewrite to jump to next code pos, if conditional statement wrong, which is only known after the sub graph is generated and the next
        //edge is added (else part)
        if (edge.cond != null && start.length > (edgeIndex._2 + 1)) {
          code(currentCodePos) = code"if(${edge.cond}) {${edge.code}; $posChanger} else {${AlgoInfo.positionVar} := ${Const(code.length)}}"
        }

        //Add wait at end, if there is a wait on that edge
        if (edge.waitEdge) {
          code(currentCodePos) = code"${code(currentCodePos)}; ${AlgoInfo.timeVar} := (${AlgoInfo.timeVar}!) + 1"
        }

        //code(currentCodePos) = code"println(${Const(currentCodePos)}); ${code(currentCodePos)}"

      })
    }

    generateCodeInner(0)

    // Add position storing for the code elements, where required
    changeCodePos.foreach(x => {
      val c = code(x._1)
      x._2.storePosRef.foreach(edgeInfoGroup => {
        // It's either all (???,-1) or non of them, so either it's a direct call or it's not, since the local graph
        // is not modified, so method inlining should be applied to all subgraphs. If there is a wait, then it should
        // be for all as well, so this makes no difference
        val edgeInfos = edgeInfoGroup.map(edgeInfo => {
          val startPos = edgeInfo.from.getNativeId
          val endPos = edgeInfo.to.getNativeId
          if (requiredSavings.contains(edgeInfo.from.getNativeId)) {
            (edgeInfo.edgeState,posEdgeSaving((startPos, endPos)), edgeInfo.positionStack, (edgeInfo.from.getNativeId, edgeInfo.to.getNativeId))
          } else {
            (edgeInfo.edgeState, -1, edgeInfo.positionStack, (edgeInfo.from.getNativeId, edgeInfo.to.getNativeId))
          }
        })


        val amountM1 = edgeInfos.count(_._2 == -1)

        assert(amountM1 == 0 || amountM1 == edgeInfos.length)

        if (amountM1 == 0) {
          assert(edgeInfos.count(_._3 == edgeInfos.head._3) == edgeInfos.length)
          val startCode = code"List[((Int, Int), Int)]()"
          val lookupT = edgeInfos.foldRight(startCode)((a, b) => code"((${Const(a._1._1)},${Const(a._1._2)}),${Const(a._2)}) :: $b")
          code(x._1) = code"$c; ${edgeInfos.head._3}.prepend($lookupT)"
        }
      })
    })


    //Rewrite variables to replaced ones
    code = code.map(x => {
      var y = x
      compiledActorGraph.variables.foreach({
        case v => {
          //Quick-fix for var types
          if(v.from != null) {
            y = y.subs(v.from).~>(code"(${v.to}!).asInstanceOf[${v.A}]")
          }
        }
      })
      y
    })



    code.toList
  }

  /**
    * Creates the class file
    * @param initParams state variables
    * @param initVars generated variables needed globally
    * @param run_until function, which overrides the run until method
    */
  def createClass(className: String, initParams: String, initVars: String, run_until: String): Unit = {
    val classString =
      s"""
          package simulation.generated

          class ${className} extends ecosim.deep.member.Actor {
            ${initParams}
              ${initVars}
              ${run_until}
        }"""
    val file = new File("simulation/src/main/scala/simulation/generated/" + className + ".scala")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(classString)
    bw.close()
  }

  def changeTypes(code: String): String = {
    var result = code
    for (cAG <- this.compiledActorGraphs) {
      for (aT <- cAG.actorTypes) {
        result = result.replace(aT.X.runtimeClass.getCanonicalName, "simulation.generated." + cAG.name)
      }
    }
    result
  }

  /**
    * Converts a list of opencode code fragments to a opencode of list of code fragments
    */
  def createCommandOpenCode(commands: List[OpenCode[Unit]]): OpenCode[List[() => Unit]] = {
    val start: OpenCode[List[() => Unit]] = code"List[() => Unit]()"
    commands.map(x => code"List(() => ${x})").foldLeft(start)((x, y) => code"$x ::: $y")
  }


  /**
    * Generates init code of one variable of type VarWrapper
    *
    * @param variable of type VarWrapper
    * @param rest     Code, where variables should be applied to
    * @tparam A type of variable
    * @tparam R return type of code
    * @return rest with bounded variable
    */
  private def initVar[A, R: CodeType](variable: AlgoInfo.VarWrapper[A], rest: OpenCode[R]): OpenCode[R] = {
    import variable.A
    code"val ${variable.to} = MutVar(${nullValue[A]}); $rest"
  }

  /**
    * Generates init code for variables of a list of VarWrappers
    *
    * @param variables list of varWrapper
    * @param after     code, where variables should be applied to
    * @tparam R return type of code
    * @return after with bounded variables
    */
  def generateMutVarInit[R: CodeType](variables: List[AlgoInfo.VarWrapper[_]], after: OpenCode[R]): OpenCode[R] = variables match {
    case Nil => code"$after"
    case (x :: xs) => {
      initVar(x, generateMutVarInit(xs, after))
    }
  }

  /**
    * Generates init code of one variable of type VarValue
    *
    * @param variable variable of type VarValue
    * @param rest     Code, where variables should be applied to
    * @tparam A type of variable
    * @tparam R return type of code
    * @return rest with bounded variable
    */
  private def initVar2[A, R: CodeType](variable: VarValue[A], rest: OpenCode[R]): OpenCode[R] = {
    code"val ${variable.variable} = ${variable.init}; $rest"
  }

  /**
    * generates init code for variables of list of type VarValue
    *
    * @param variables list of varvalue
    * @param after     code, where variables should be applied to
    * @tparam R return type of code
    * @return after with bounded variables
    */
  def generateVarInit[R: CodeType](variables: List[VarValue[_]], after: OpenCode[R]): OpenCode[R] = variables match {
    case Nil => code"$after"
    case (x :: xs) => {
      initVar2(x, generateVarInit(xs, after))
    }
  }


  def createInit(code: String): Unit = {
    val classString =
      s"""
          package simulation.generated

          object InitData  {
            def initActors: List[ecosim.deep.member.Actor] = {${changeTypes(code)}}
          }
        """
    val file = new File("simulation/src/main/scala/simulation/generated/InitData.scala")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(classString)
    bw.close()
  }

}
