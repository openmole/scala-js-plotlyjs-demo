import sbt.IO

name := "scala-js-plotlyjs-demo"

scalaVersion := "3.5.0"

val poltlyjsVersion = "1.8.1"
val scaladgetVersion = "1.11.0"
val laminarVersion = "16.0.0"

resolvers += Resolver.bintrayRepo("definitelyscala", "maven")
//resolvers += Resolver.sonatypeOssRepos _//sonatypeRepo("snapshots")
resolvers += Resolver.jcenterRepo

lazy val runDemo = taskKey[Unit]("runDemo")

lazy val demo = project.in(
  file(".")
) enablePlugins (ScalaJSBundlerPlugin) settings (
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
  libraryDependencies += "com.raquo" %%% "laminar" % laminarVersion,
  libraryDependencies += "org.openmole.scaladget" %%% "bootstrapnative" % scaladgetVersion,
  libraryDependencies += "org.openmole.scaladget" %%% "highlightjs" % scaladgetVersion,
  libraryDependencies += "org.openmole.scaladget" %%% "svg" % scaladgetVersion,
  libraryDependencies += "org.openmole" %%% "scala-js-plotlyjs" % poltlyjsVersion,
  libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.4.2",
  scalaJSLinkerConfig := scalaJSLinkerConfig.value.withSourceMap(false),
  webpackNodeArgs := Seq("--openssl-legacy-provider"),
  scalaJSUseMainModuleInitializer := true,
  Compile / npmDependencies += "plotly.js" -> "2.13.3",
  runDemo := {
    val demoResource = (Compile / resourceDirectory).value
    val jsBuild = (Compile / fastOptJS / webpack).value.head.data

    IO.copyFile(jsBuild, target.value / "js/demoplotly.js")
    // IO.copyFile(crossTarget.value / s"${name.value}-jsdeps.js", target.value / "js/deps.js")
    IO.copyDirectory(demoResource, target.value)
  }
)
