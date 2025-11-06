package esmeta.test262.jalangi

import esmeta.test262.*
import esmeta.test262.util.*
import esmeta.util.{ConcurrentPolicy as CP}
import esmeta.util.SystemUtils.*
import java.io.PrintStream

object Jalangi {

  def test(paths: Option[List[String]])(using test262: Test262): Unit = {

    // pre-jobs
    val tests: List[Test] = test262.getTests(paths, features = None)
    val (targetTests, removed) = test262.testFilter(tests, test262.withYet)
    val multiple = targetTests.length > 1

    new Test262Runner(
      msg = "Jalangi Test262 Test",
      targets = targetTests,
      notSupported = removed,
      concurrent = CP.Single,
      showProgressBar = true,
    ) {

      lazy val getName: (Test, Int) => String = (t, _) => t.relName

      def runTest(test: Test): Unit = {
        mkdir("/tmp/esmeta-jalangi/")
        val tmpFilePath =
          s"/tmp/esmeta-jalangi/${test.relName.replace("/", "_")}"
        val (ast, code) = test262.loadTest(test.absPath)
        dumpFile(code, tmpFilePath)
        val jalangiCmd =
          s"""$JALANGI_COMMAND
           | --inlineIID
           | --inlineSource
           | --analysis $ANALYSIS_FILE_PATH
           | $tmpFilePath
           |""".stripMargin.replaceAll("\n", " ")

        val str =
          try {
            println(s"Executing Jalangi command: $jalangiCmd")
            executeCmd(jalangiCmd)
            // maybe test includes new features not supported by Jalangi
          } catch {
            case e: Throwable =>
              println(s"Error in Jalangi Test262 Test: ${e.getStackTrace()}")
              throw e
          }

        println(s"Jalangi output:\n${str}")

        val byteStream = new java.io.ByteArrayOutputStream()
        val outputStream = new java.io.PrintStream(byteStream)

        println("Running Traced Interpreter...")

        Console.withOut(outputStream) {
          TracedInterpreter(
            test262.cfg.init.from(code, ast),
          ).result
        }

        println("Finished Traced Interpreter.")

        val output = { outputStream.close(); byteStream.toString() }

        println(s"Traced Interpreter output:\n${output}")

        if (str == output)
          println(
            s"${Console.GREEN}Test ${test.relName} passed.${Console.RESET}",
          )
        else
          println(s"${Console.RED}Test ${test.relName} failed.${Console.RESET}")

      }

      override def errorHandler(
        error: Throwable,
        summary: Summary,
        name: String,
      ): Unit = super.errorHandler(error, summary, name)

    }.result
  }

  val JALANGI_COMMAND: String = {
    val home = sys.env.getOrElse(
      "JALANGI_HOME",
      throw new RuntimeException("JALANGI_HOME not set"),
    )
    s"node ${home}/src/js/commands/jalangi.js"
  }
  val ANALYSIS_FILE_PATH: String = {
    val home = sys.env.getOrElse(
      "ESMETA_HOME",
      throw new RuntimeException("ESMETA_HOME not set"),
    )
    val path = java.nio.file.Paths.get(home, "analysis.js")
    if (!java.nio.file.Files.exists(path))
      throw new java.io.FileNotFoundException(s"${path} not found")
    path.toString
  }

}
