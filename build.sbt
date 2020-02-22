name := "koktai-scala"

version := "0.3"

scalaVersion := "2.12.10"

mainClass in (Compile, run) := Some("koktai.ExporterTEI")
    
libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6",
    //"org.scala-lang" % "scala-xml" % "2.11.0-M4",
    //"com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
    //"ch.qos.logback" %  "logback-classic" % "1.1.7",
    //"com.lihaoyi" %% "upickle" % "0.4.4",
    // "com.lihaoyi" % "upickle_2.11" % "0.4.4"
    "io.suzaku" %% "boopickle" % "1.2.6"
    )


