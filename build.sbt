name := "scalajsir-calc"
scalaVersion := "2.11.8"
version := "0.1-SNAPSHOT"

scalacOptions ++= Seq(
    "-deprecation", "-feature", "-unchecked", "-encoding", "utf-8")

val scalaJSVersion = "0.6.8"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-js" %% "scalajs-tools" % scalaJSVersion,
  "org.scala-js" %% "scalajs-js-envs" % scalaJSVersion,
  "com.lihaoyi" %% "fastparse" % "0.3.7",
  "org.scalatest" % "scalatest_2.11" % "2.2.6",
  "com.novocode" % "junit-interface" % "0.9" % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-a")

// Some magic to add the scalajs-library.jar in the resources
ivyConfigurations += config("linkingdeps").hide
libraryDependencies +=
  "org.scala-js" %% "scalajs-library" % scalaJSVersion % "linkingdeps"
resourceGenerators in Compile += Def.task {
  val jars = update.value.select(configurationFilter("linkingdeps"))
  for (jar <- jars) yield {
    val dest = (resourceManaged in Compile).value / "calc" / jar.getName
    IO.copyFile(jar, dest, preserveLastModified = true)
    dest
  }
}.taskValue
