package ecosim.deep.codegen

import java.io.{BufferedWriter, File, FileWriter}

import ecosim.deep.IR.Predef._
import ecosim.deep.IR
import ecosim.deep.member.ActorType

class InitCreation(code: OpenCode[Any], actorTypes: List[ActorType[_]]) {

  def run(): Unit = {
    createObject(IR.showScala(code.rep))
  }

  def createObject(code: String): Unit = {
    val classString =
      s"""
          package simulation.generated

          object InitData  {
            def initActors: List[simulation.core.Actor] = {${changeTypes(code)}}
          }
        """
    val file = new File("simulation/src/main/scala/simulation/generated/InitData.scala")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(classString)
    bw.close()
  }

  def changeTypes(code: String): String = {
    var result = code
    for (aT <- actorTypes) {
      result = result.replace(aT.X.runtimeClass.getCanonicalName, "simulation.generated." + aT.name)
    }
    result
  }
}
