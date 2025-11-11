package esmeta.test262.jalangi

import esmeta.test262.*
import esmeta.test262.util.*
import esmeta.util.{ConcurrentPolicy as CP}
import esmeta.util.SystemUtils.*
import java.io.PrintStream
import esmeta.error.NotSupported
import scala.concurrent.TimeoutException
import scala.concurrent.duration.*
import esmeta.util.BaseUtils.getMessage
import esmeta.LOG_DIR

object Jalangi {

  def test(
    paths: Option[List[String]],
    log: Boolean = true,
    timeLimit: Option[Int] = None,
  )(using
    test262: Test262,
  ): Summary = {

    // pre-jobs
    val tests: List[Test] = test262.getTests(paths, features = None)
    val (targetTests, removed) = test262.testFilter(tests, test262.withYet)
    val multiple = targetTests.length > 1
    def printlnIfSingle(s: => String): Unit = if (!multiple) println(s)

    mkdir("/tmp/esmeta-jalangi/")

    def isES5(filename: String): Boolean = {
      val (exitcode, str) = executeCmdNonZero(
        s"acorn --ecma5 $filename",
      )
      printlnIfSingle(s"ESLint output:\n${str.slice(0, 20)}...")
      exitcode == 0
    }

    val runner = {
      new Test262Runner(
        msg = "Jalangi Test262 Test",
        targets = targetTests,
        notSupported = removed,
        concurrent = CP.Fixed(16),
        showProgressBar = true,
      ) {

        lazy val getName: (Test, Int) => String = (t, _) => t.relName

        def runTest(test: Test): Unit = {

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

          printlnIfSingle(s"Running Jalangi ${jalangiCmd}")

          if (!isES5(tmpFilePath)) {
            throw NotSupported("not es5")
          }

          val str =
            try {
              // printlnIfSingle(s"Executing Jalangi command: $jalangiCmd")
              val s =
                executeCmdTimeout(jalangiCmd, duration = 60.seconds).getOrElse(
                  throw java.util.concurrent.TimeoutException(
                    "Jalangi timed out, maybe because of ES6+ features?",
                  ),
                )
              if (s.startsWith("Failed to instrument"))
                throw NotSupported(
                  "The test may include new features not supported by Jalangi",
                )
              s
              // maybe test includes new features not supported by Jalangi
            } catch {
              case e: Throwable =>
                printlnIfSingle(
                  s"Error in Jalangi Test262 Test: ${e.getStackTrace()}",
                )
                throw e;
            }
          // printlnIfSingle(s"Jalangi output:\n${str}")

          val byteStream = new java.io.ByteArrayOutputStream()
          val outputStream = new java.io.PrintStream(byteStream)

          // printlnIfSingle("Running Traced Interpreter...")

          Console.withOut(outputStream) {
            TracedInterpreter(
              test262.cfg.init.from(code, ast),
              timeLimit = timeLimit,
              analysis = new JalangiAnalysis(),
            ).result
          }

          // printlnIfSingle("Finished Traced Interpreter.")

          val output = { outputStream.close(); byteStream.toString() }

          // printlnIfSingle(s"Traced Interpreter output:\n${output}")

          printlnIfSingle("===================== Diff ====================")
          printlnIfSingle(Diff.get(str, output))
          printlnIfSingle("==============================================")

          if (str == output)
            printlnIfSingle(
              s"${Console.GREEN}Test ${test.relName} passed.${Console.RESET}",
            )
          // summary.pass.add(test.relName)
          // it is added automatically?
          else
            printlnIfSingle(
              s"${Console.RED}Test ${test.relName} failed.${Console.RESET}",
            )
            throw new Exception("Test failed")

        }

        override def errorHandler(
          error: Throwable,
          summary: Summary,
          name: String,
          test: Test,
        ): Unit =
          if (multiple) error match
            case NotSupported(reasons) =>
              summary.notSupported.add(name, reasons)
            case _: TimeoutException =>
              println(s"[TIMEOUT] $name")
              summary.timeout.add(name)
            case e: Throwable =>
              // if (log)
              //   logPW.println(s"[FAIL   ] $name")
              //   logPW.println(e.getStackTrace.mkString(LINE_SEP))
              //   logPW.flush

              // synchronized {
              //   println(s"[FAIL   ] $name: ${getMessage((e))}")
              // }
              summary.fail.add(name, getMessage((e)))
          else throw error

        override def postJob: Summary = {
          val summary = super.postJob
          val logDir = s"${LOG_DIR}/jalangi"

          // logging after tests
          if (log)
            summary.dumpTo(logDir)
            dumpFile(
              s"Jalangi test summary",
              summary.toString,
              s"$logDir/summary",
            )

          // if exists(logDir) then
          // createSymLink(symlink, logDir, overwrite = true)
          // logPW.close()
          summary
        }
      }
    }

    runner.result
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
