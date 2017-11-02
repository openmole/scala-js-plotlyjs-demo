import java.io.PrintWriter

import sbt.IO
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

name := "scala-js-plotlyjs-demo"

scalaVersion := "2.12.4"

resolvers += Resolver.bintrayRepo("definitelyscala", "maven")
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.jcenterRepo

lazy val runDemo = taskKey[Unit]("runDemo")

lazy val demo = project.in(file(".")) enablePlugins (ScalaJSPlugin) settings(
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2",
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.4",
  libraryDependencies += "fr.iscpif" %%% "scaladget" % "0.9.6-SNAPSHOT",
  libraryDependencies += "com.definitelyscala" %%% "scala-js-plotlyjs" % "1.1.3",
  libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.1.2",
  runDemo := {
    val demoTarget = target.value
    val demoResource = (resourceDirectory in Compile).value
    
    val demoJS = (fastOptJS in Compile).value

    IO.copyFile(demoJS.data, demoTarget / "js/demo.js")

    IO.copyFile(demoResource / "plotly-demo.html", demoTarget / "plotly-demo.html")
    IO.copyDirectory(demoResource / "js", demoTarget / "js")
    IO.copyDirectory(demoResource / "css", demoTarget / "css")
    IO.copyDirectory(demoResource / "data", demoTarget / "data")
  }

)