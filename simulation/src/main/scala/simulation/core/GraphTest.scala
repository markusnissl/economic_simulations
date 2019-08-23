package simulation.core

import java.io.File

import guru.nidi.graphviz.attribute.{Color, RankDir, Style}
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.Graph
import guru.nidi.graphviz.model.Factory

object GraphTest extends App {
  val g: Graph = Factory.graph("example1")
    .directed().graphAttr()
    .`with`(RankDir.LEFT_TO_RIGHT)
    .`with`(
      Factory.node("a").link("b").`with`(Color.RED),
      Factory.node("d").link("b").`with`(Color.RED),
      Factory.node("b").link(Factory.to(Factory.node("c")).`with`(Style.DASHED)).link("b")
    )

  //Graphviz.fromGraph(g).height(100).render(Format.PNG).toFile(new File("example/ex1.png"));
}
