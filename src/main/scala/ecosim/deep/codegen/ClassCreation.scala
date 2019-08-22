package ecosim.deep.codegen

import java.io.{BufferedWriter, File, FileWriter}

import ecosim.deep.IR.Predef._
import ecosim.deep.IR
import ecosim.deep.algo.AlgoInfo
import ecosim.deep.member.ActorType
import squid.lib.MutVar
import scala.collection.mutable.ListBuffer

class ClassCreation(actorType: ActorType[_], actorTypes: List[ActorType[_]]) extends Codegen(actorType) {


  override def run(): Unit = {

    val self = actorType.self.toCode.toString().substring(5).dropRight(1)
    val commands = this.createLists()
    val code = this.createCommandOpenCode(commands)
    val codeWithInit = this.generateVarInit(variables2, this.generateMutVarInit(AlgoInfo.variables,
      code"""val ${AlgoInfo.returnValue} = MutVar(null)
              val ${AlgoInfo.positionStack} = ListBuffer[Int]()
              val ${AlgoInfo.responseMessage} = MutVar(null)
              val ${AlgoInfo.timeVar} = MutVar(0)
              val ${AlgoInfo.positionVar} = MutVar(0)
              val getCommands = () => $code
              val commands = getCommands()
              ecosim.deep.algo.Instructions.splitter
              (until: Int) => {
                while ((${AlgoInfo.timeVar}!) <= until && (${AlgoInfo.positionVar}!) < commands.length) {
                  val command = commands((${AlgoInfo.positionVar}!))
                  command()
                  ${AlgoInfo.positionVar} := (${AlgoInfo.positionVar}!) + 1
                }
                ${actorType.self}
              }
          """))
    val steps = changeTypes(IR.showScala(codeWithInit.rep).replace(self, "this"))

    val parts = steps.split("""ecosim\.deep\.algo\.Instructions\.splitter\(\);""")
    val initVars = parts(0).substring(2)
    //val commandList = "val commands= {" + parts(1)
    val run_until = "override def run_until" + parts(1).trim().substring(1).replaceFirst("=>", ": simulation.core.Actor = ").dropRight(1).trim.dropRight(1)

    var initParams = ""
    for (s <- actorType.states) {
      initParams = initParams + "var " + s.sym.name + ": " + changeTypes(s.tpe.rep.toString) + " = " + changeTypes(IR.showScala(s.init.rep)) + "\n"
    }
    createClass(initParams, initVars, run_until)
  }

  def changeTypes(code: String): String = {
    var result = code
    for (aT <- actorTypes) {
      result = result.replace(aT.X.runtimeClass.getCanonicalName, "simulation.generated." + aT.name)
    }
    result
  }

  def createClass(initParams: String, initVars: String, run_until: String): Unit = {
    val classString =
      s"""
          package simulation.generated

          class ${actorType.name} extends simulation.core.Actor {
            ${initParams}
              ${initVars}
              ${run_until}
        }"""
    val file = new File("simulation/src/main/scala/simulation/generated/" + actorType.name + ".scala")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(classString)
    bw.close()
  }

}
