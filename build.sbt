ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

lazy val matrix = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file(".")).
  settings(
    name := "matrix",
    version := "0.1.1",
    scalaVersion := "3.1.3",
    scalacOptions ++=
      Seq(
        "-deprecation", "-feature", "-unchecked",
        "-language:postfixOps", "-language:implicitConversions", "-language:existentials", "-language:dynamics",
        "-explain"
      ),
    organization := "io.github.edadma",
    githubOwner := "edadma",
    githubRepository := "matrix",
    mainClass := Some(s"${organization.value}.${name.value}.Main"),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "table" % "1.0.1",
      "io.github.edadma" %%% "numbers" % "0.1.2" % "test",
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
  ).
  jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
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
