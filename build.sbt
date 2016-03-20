name := "scalajsir-calc"
scalaVersion := "2.11.8"
version := "0.1-SNAPSHOT"

val scalaJSVersion = "0.6.8"

libraryDependencies ++= Seq(
  "org.scala-js" %% "scalajs-tools" % scalaJSVersion,
  "org.scala-js" %% "scalajs-js-envs" % scalaJSVersion,
  
  "com.novocode" % "junit-interface" % "0.9" % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-a")
