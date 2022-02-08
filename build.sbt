import sbtassembly.AssemblyPlugin.defaultUniversalScript

ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "3.1.0"
ThisBuild / organization := "esmeta"
ThisBuild / scalacOptions := Seq(
  "-language:implicitConversions", // allow implicit conversions
  "-deprecation", // emit warning and location for usages of deprecated APIs
  "-explain", // explain errors in more detail
  "-explain-types", // explain type errors in more detail
  "-feature", // emit warning and location for usages of features that should be imported explicitly
  "-unchecked", // enable additional warnings where generated code depends on assumptions
  "-Xmigration", // warn about constructs whose behavior may have changed since version
)
ThisBuild / javacOptions ++= Seq(
  "-encoding",
  "UTF-8",
)

// automatic reload build.sbt
Global / onChangedBuildSource := ReloadOnSourceChanges

// assembly setting
ThisBuild / assemblyPrependShellScript :=
  Some(defaultUniversalScript(shebang = false))

// project root
lazy val root = project
  .in(file("."))
  .settings(
    name := "esmeta",

    // libraries
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.1",
      "io.circe" %% "circe-generic" % "0.14.1",
      "io.circe" %% "circe-parser" % "0.14.1",
      "org.scalatest" %% "scalatest" % "3.2.10" % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
      "org.apache.commons" % "commons-text" % "1.9",
      "org.jsoup" % "jsoup" % "1.14.3",
    ),

    // Copy all managed dependencies to <build-root>/lib_managed/ This is
    // essentially a project-local cache.  There is only one lib_managed/ in
    // the build root (not per-project).
    retrieveManaged := true,

    // set the main class for 'sbt run'
    Compile / mainClass := Some("esmeta.ESMeta"),

    // test setting
    Test / testOptions += Tests
      .Argument("-fDG", baseDirectory.value + "/tests/detail"),
    Test / parallelExecution := true,

    // assembly setting
    assembly / test := {},
    assembly / assemblyOutputPath := file("bin/esmeta"),
  )

// create the `.completion` file for autocompletion in shell
lazy val genCompl = taskKey[Unit]("generate autocompletion file (.completion)")
genCompl := (root / Compile / runMain).toTask(" esmeta.util.GenCompl").value

// build with genCompl and assembly
lazy val build = taskKey[Unit]("my test task")
build := {
  genCompl.value
  (root / assembly / assembly).value
}
