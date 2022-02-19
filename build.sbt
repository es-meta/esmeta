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

// size
lazy val tinyTest = taskKey[Unit]("Launch tiny tests (maybe milliseconds)")
lazy val smallTest = taskKey[Unit]("Launch small tests (maybe seconds)")
lazy val middleTest = taskKey[Unit]("Launch middle tests (maybe minutes)")
lazy val largeTest = taskKey[Unit]("Launch large tests (may hours)")

// spec
lazy val specTest = taskKey[Unit]("Launch spec tests")
lazy val specStringifyTest =
  taskKey[Unit]("Launch stringify tests for spec (tiny)")

// lang
lazy val langTest = taskKey[Unit]("Launch lang tests")
lazy val langStringifyTest =
  taskKey[Unit]("Launch stringify tests for lang (tiny)")

// ir
lazy val irTest = taskKey[Unit]("Launch ir tests")
lazy val irStringifyTest = taskKey[Unit]("Launch stringify tests for ir (tiny)")

// interp
lazy val interpTest = taskKey[Unit]("Launch interp tests")
lazy val interpStringifyTest =
  taskKey[Unit]("Launch stringify tests for interp (tiny)")
lazy val interpEvalTest = taskKey[Unit]("Launch eval tests for interp (tiny)")

// js
lazy val jsTest = taskKey[Unit]("Launch js tests")
lazy val jsParseTest = taskKey[Unit]("Launch parse tests for js (small)")

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

    /** tasks for tests */
    // basic tests
    test := (Test / testOnly)
      .toTask(
        List(
          "*TinyTest",
          "*SmallTest",
        ).mkString(" ", " ", ""),
      )
      .value,
    // size
    tinyTest := (Test / testOnly).toTask(" *TinyTest").value,
    smallTest := (Test / testOnly).toTask(" *SmallTest").value,
    middleTest := (Test / testOnly).toTask(" *MiddleTest").value,
    largeTest := (Test / testOnly).toTask(" *LargeTest").value,
    // spec
    specTest := (Test / testOnly).toTask(" *.spec.*Test").value,
    specStringifyTest := (Test / testOnly)
      .toTask(" *.spec.Stringify*Test")
      .value,
    // lang
    langTest := (Test / testOnly).toTask(" *.lang.*Test").value,
    langStringifyTest := (Test / testOnly)
      .toTask(" *.lang.Stringify*Test")
      .value,
    // ir
    irTest := (Test / testOnly).toTask(" *.ir.*Test").value,
    irStringifyTest := (Test / testOnly).toTask(" *.ir.Stringify*Test").value,
    // interp
    interpTest := (Test / testOnly).toTask(" *.interp.*Test").value,
    interpStringifyTest := (Test / testOnly)
      .toTask(" *.interp.Stringify*Test")
      .value,
    interpEvalTest := (Test / testOnly).toTask(" *.interp.Eval*Test").value,
    // js
    jsTest := (Test / testOnly).toTask(" *.js.*Test").value,
    jsParseTest := (Test / testOnly).toTask(" *.js.parse*Test").value,
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
