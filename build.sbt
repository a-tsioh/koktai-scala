name := "koktai-scala"

version := "0.1"

scalaVersion := "2.11.6"

mainClass in (Compile, run) := Some("koktai.Main")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scala-lang" % "scala-xml" % "2.11.0-M4",
  //"com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
  "ch.qos.logback" %  "logback-classic" % "1.1.7"
)