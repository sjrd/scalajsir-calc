name := "scalajsir-calc"
scalaVersion := "2.13.1"
version := "0.1-SNAPSHOT"

scalacOptions ++= Seq(
    "-deprecation", "-feature", "-unchecked", "-encoding", "utf-8")

val scalaJSVersion = "1.0.1"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-js" %% "scalajs-linker" % scalaJSVersion,
  "org.scala-js" %% "scalajs-env-nodejs" % scalaJSVersion,
  "com.lihaoyi" %% "fastparse" % "2.1.3",

  "com.novocode" % "junit-interface" % "0.9" % "test",
  "org.scala-js" %% "scalajs-js-envs-test-kit" % scalaJSVersion,
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-a")

fork := true

// Some magic to pass -Dcalc.scalajslib=/path/to/scalajs-library.jar
val LinkingDeps = config("linkingdeps").hide
ivyConfigurations += LinkingDeps
libraryDependencies +=
  "org.scala-js" %% "scalajs-library" % scalaJSVersion % "linkingdeps"
javaOptions += {
  val jars = update.value.select(configurationFilter("linkingdeps"))
  val scalajslibJar = jars.find(_.getName.startsWith("scalajs-library")).getOrElse {
    throw new NoSuchElementException(
        s"Did not find scalajs-library in $jars")
  }
  "-Dcalc.scalajslib=" + scalajslibJar.getAbsolutePath.toString
}
