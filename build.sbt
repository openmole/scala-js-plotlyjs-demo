
import sbt.IO

name := "scala-js-plotlyjs-demo"

scalaVersion := "2.13.5"
crossScalaVersions := Seq("2.12.13", "2.13.5")

val poltlyjsVersion = "1.5.6"
val scaladgetVersion = "1.9.0"
val laminarVersion = "0.12.2"

resolvers += Resolver.bintrayRepo("definitelyscala", "maven")
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.jcenterRepo

lazy val runDemo = taskKey[Unit]("runDemo")

lazy val demo = project.in(file(".")) enablePlugins (ScalaJSBundlerPlugin, JSDependenciesPlugin) settings(
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0",
  libraryDependencies += "com.raquo" %%% "laminar" % laminarVersion,
  libraryDependencies += "org.openmole.scaladget" %%% "bootstrapnative" % scaladgetVersion,
  libraryDependencies += "org.openmole.scaladget" %%% "highlightjs" % scaladgetVersion,
  libraryDependencies += "org.openmole.scaladget" %%% "svg" % scaladgetVersion,
  libraryDependencies += "org.openmole" %%% "scala-js-plotlyjs" % poltlyjsVersion ,
  libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.2.6",
  scalaJSUseMainModuleInitializer := true,
  runDemo := {
    val demoResource = (Compile / resourceDirectory).value
    val jsBuild = (Compile / fastOptJS / webpack ).value.head.data

    IO.copyFile(jsBuild, target.value / "js/demoplotly.js")
    IO.copyFile(crossTarget.value / s"${name.value}-jsdeps.js", target.value / "js/deps.js")
    IO.copyDirectory(demoResource, target.value)
  }

)
