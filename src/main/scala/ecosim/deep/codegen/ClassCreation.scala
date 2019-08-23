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
    val codeWithInit = this.generateVarInit(variables, this.generateMutVarInit(AlgoInfo.variables,
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

    //Needed to split, so that a function can be extracted from the code, to write everything as class variables
    val parts = steps.split("""ecosim\.deep\.algo\.Instructions\.splitter\(\);""")
    val initVars = parts(0).substring(2)
    //This ugly syntax is needed to replace the received code with a correct function definition
    val run_until = "override def run_until" + parts(1).trim().substring(1).replaceFirst("=>", ": ecosim.deep.member.Actor = ").dropRight(1).trim.dropRight(1)

    // Converts all initParams to state variables again
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

  /**
    * Creates the class file
    * @param initParams state variables
    * @param initVars generated variables needed globally
    * @param run_until function, which overrides the run until method
    */
  def createClass(initParams: String, initVars: String, run_until: String): Unit = {
    val classString =
      s"""
          package simulation.generated

          class ${actorType.name} extends ecosim.deep.member.Actor {
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
