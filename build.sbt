import java.io.PrintWriter

import sbt.IO
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import execnpm.NpmDeps.Dep

name := "scala-js-plotlyjs-demo"

scalaVersion := "2.13.1"

resolvers += Resolver.bintrayRepo("definitelyscala", "maven")
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.jcenterRepo

lazy val runDemo = taskKey[Unit]("runDemo")

lazy val demo = project.in(file(".")) enablePlugins (ExecNpmPlugin) settings(
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0",
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.1",
  libraryDependencies += "org.openmole.scaladget" %%% "bootstrapnative" % "1.3.0",
  libraryDependencies += "org.openmole" %%% "scala-js-plotlyjs" % "1.5.1",
  libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.2.1",

  runDemo := {
    val demoTarget = target.value
    val demoResource = (resourceDirectory in Compile).value

    val demoJS = (fastOptJS in Compile).value

    IO.copyFile(demoJS.data, demoTarget / "js/demo.js")
    IO.copyFile(dependencyFile.value, demoTarget / "js/deps.js")

    IO.copyFile(demoResource / "plotly-demo.html", demoTarget / "plotly-demo.html")
    IO.copyDirectory(demoResource / "js", demoTarget / "js")
    IO.copyDirectory(demoResource / "css", demoTarget / "css")
    IO.copyDirectory(demoResource / "data", demoTarget / "data")
  }

)
