import java.io.PrintWriter

import sbt.IO
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

name := "scala-js-plotlyjs-demo"

scalaVersion := "2.12.11"

resolvers += Resolver.bintrayRepo("definitelyscala", "maven")
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.jcenterRepo

lazy val runDemo = taskKey[Unit]("runDemo")

lazy val demo = project.in(file(".")) enablePlugins (ExecNpmPlugin) settings(
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.8",
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.7.0",
  libraryDependencies += "fr.iscpif.scaladget" %%% "bootstrapnative" % "1.2.7",
  libraryDependencies += "com.definitelyscala" %%% "scala-js-plotlyjs" % "1.2",
  libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.2.1",

  runDemo := {
    val demoTarget = target.value
    val demoResource = (resourceDirectory in Compile).value

    val demoJS = (fullOptJS in Compile).value

    IO.copyFile(demoJS.data, demoTarget / "js/demo.js")
    IO.copyFile(dependencyFile.value, demoTarget / "js/deps.js")

    IO.copyFile(demoResource / "plotly-demo.html", demoTarget / "plotly-demo.html")
    IO.copyDirectory(demoResource / "js", demoTarget / "js")
    IO.copyDirectory(demoResource / "css", demoTarget / "css")
    IO.copyDirectory(demoResource / "data", demoTarget / "data")
  }

)
