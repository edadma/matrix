lazy val matrix = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file(".")).
  settings(
    name := "matrix",
    version := "0.1.0-snapshot.4",
    scalaVersion := "2.13.5",
    scalacOptions ++=
      Seq(
        "-deprecation", "-feature", "-unchecked",
        "-language:postfixOps", "-language:implicitConversions", "-language:existentials", "-language:dynamics",
        "-Xasync"
      ),
    organization := "xyz.hyperreal",
    mainClass := Some("xyz.hyperreal.matrix.Main"),
    Test / mainClass := Some("xyz.hyperreal.matrix.Main"),  // comment out for unit testing
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.5" % "test",
    libraryDependencies ++= Seq(
      "xyz.hyperreal" %%% "table" % "1.0.0-snapshot.3",
      "xyz.hyperreal" %%% "numbers" % "0.1.0" % "test",
    ),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
  ).
  jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.0.0" % "provided",
  ).
  nativeSettings(
    nativeLinkStubs := true
  ).
  jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    Test / scalaJSUseMainModuleInitializer := true,
    Test / scalaJSUseTestModuleInitializer := false,
//    Test / scalaJSUseMainModuleInitializer := false,
//    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer := true,
  )
