lazy val root = (project in file(".")).settings(
  name := "economic_simulations",
  organization := "ch.epfl.data",
  version := "1.0",
  scalaVersion := "2.12.8"
)

// libraryDependencies += "com.quantifind" %% "wisp" % "0.0.4"

libraryDependencies  ++= Seq(
//  "com.github.fommil.netlib" % "all" % "1.1.2",
  "org.scalanlp" %% "breeze" % "0.13.2",
//  "org.scalanlp" %% "breeze-natives" % "0.12",
  "org.scalanlp" %% "breeze-viz" % "0.13.2",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.apache.spark" %% "spark-core" % "2.4.3",
  "com.chuusai" %% "shapeless" % "2.3.3"
)

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "ch.epfl.data" %% "squid" % "0.4.0-SNAPSHOT"
