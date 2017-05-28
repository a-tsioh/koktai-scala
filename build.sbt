enablePlugins(ScalaJSPlugin)

scalaJSModuleKind := ModuleKind.CommonJSModule

val app = crossProject.crossType(CrossType.Full).in(file("./app")).settings(
  name := "koktai-scala",
  version := "0.2",
  scalaVersion := "2.12.2",
  //unmanagedSourceDirectories in Compile +=
  //  baseDirectory.value  / "shared" / "main" / "scala",
  libraryDependencies ++= Seq(

  )
)
  .jsSettings(
    scalaVersion := "2.12.2",
   // scalaJSModuleKind := ModuleKind.CommonJSModule,
    //scalaJSUseMainModuleInitializer := true,
    //mainClass := Some("koktai.jsMain"),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.2",
      "io.suzaku" %%% "boopickle" % "1.2.6"
    )
  )
  .jvmSettings(
    mainClass in (Compile, run) := Some("koktai.Main"),
    scalaVersion := "2.12.2",
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
  )

lazy val js = app.js

lazy val jvm = app.jvm