package esmeta.test262.jalangi

import esmeta.test262.*
import esmeta.test262.util.*
import esmeta.util.{ConcurrentPolicy as CP}
import esmeta.util.SystemUtils.*
import java.io.PrintStream
import esmeta.error.NotSupported
import esmeta.es.*
import scala.concurrent.TimeoutException
import scala.concurrent.duration.*
import esmeta.util.BaseUtils.getMessage
import esmeta.LOG_DIR

class Jalangi(
  paths: Option[List[String]],
  log: Boolean = true,
  timeLimit: Option[Int] = None,
  noCompare: Boolean = false,
  nodeprof: Boolean = true,
)(using
  test262: Test262,
) { jalangi =>

  // pre-jobs
  lazy val tests: List[Test] = test262.getTests(paths, features = None)
  lazy val (targetTests, removed) = test262.testFilter(tests, test262.withYet)
  lazy val multiple = targetTests.length > 1
  def printlnIfSingle(s: => String): Unit = if (!multiple) println(s)

  lazy val TEMP_FILE_BASE =
    s"$LOG_DIR/tempfiles-esmetajalangi" // can't use /tmp/esmeta-jalangi/ cause of docker volume

  def test: Summary = {
    mkdir(TEMP_FILE_BASE)

    executeCmdNonZero(
      s"npm run build",
      dir = CHECK_SCRIPT_DIR,
    ) match
      case (a, b, c) =>
        printlnIfSingle(s"Checkscript build output[${a}]:\n${b}\n${c}")

    val runner = {
      new Test262Runner(
        msg = "Jalangi Test262 Test",
        targets = targetTests,
        notSupported = removed,
        concurrent = CP.Fixed(8),
        showProgressBar = true,
      ) {

        lazy val getName: (Test, Int) => String = (t, _) => t.relName

        def runTest(test: Test): Unit = {

          val tmpFilePath =
            s"$TEMP_FILE_BASE/${test.relName.replace("/", "_")}"
          val (ast, code) = test262.loadTest(test.absPath)
          dumpFile(code, tmpFilePath)

          printlnIfSingle(s"Running Jalangi for ${tmpFilePath}")

          Aux.checkSyntax(tmpFilePath) match
            case (false, str, err) => throw NotSupported(err)
            case _                 => ()

          // run Traced Interpreter first, so not supported features can be caught
          val esmetaOutput = Aux.runESMetaInterp(code, ast, timeLimit)

          Aux.runNodeJs(tmpFilePath) match
            case false => throw NotSupported("Does not pass Node.js")
            case true  => ()

          val (jalangiOutput, jalangiErr) = Aux.runJalangi(tmpFilePath)
          lazy val passJalangi = jalangiOutput == esmetaOutput

          printlnIfSingle("============ Diff (esmeta-jalangi) ===========")
          printlnIfSingle(Diff.get(esmetaOutput, jalangiOutput))
          printlnIfSingle("==============================================")
          printlnIfSingle(s"Jalangi error output:\n${jalangiErr}")

          val (npOutput, npErr) = Aux.runNodeProf(
            analysisPath = ANALYSIS_FILE_PATH_COPIED_TO_TMP,
            testPath = tmpFilePath,
          )
          lazy val passNodeProf =
            if nodeprof then npOutput == esmetaOutput else true

          if (nodeprof) {
            printlnIfSingle("======== Diff (esmeta-nodeprof) =======")
            printlnIfSingle(Diff.get(esmetaOutput, npOutput))
            printlnIfSingle("=======================================")
            printlnIfSingle(s"NodeProf error output:\n${npErr}")
          }

          // val passNodeProf = true // temporarily disable NodeProf comparison

          if (passJalangi && passNodeProf)
            printlnIfSingle(
              s"${Console.GREEN}Test ${test.relName} passed.${Console.RESET}",
            )
          // summary.pass.add(test.relName)
          // it is added automatically?
          else if (!passJalangi)
            printlnIfSingle(
              s"${Console.RED}Test ${test.relName} failed. (Jalangi)${Console.RESET}",
            )
            throw new Exception("Test failed for Jalangi")
          else if (!passNodeProf)
            printlnIfSingle(
              s"${Console.RED}Test ${test.relName} failed. (NodeProf)${Console.RESET}",
            )
            throw new Exception("Test failed for NodeProf")
          else
            printlnIfSingle(
              s"${Console.RED}Test ${test.relName} failed. (Jalangi & NodeProf)${Console.RESET}",
            )
            throw new Exception("Test failed for both Jalangi and NodeProf")
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

  val NODEPROF_HOME: String = {
    sys.env.getOrElse(
      "NODEPROF_HOME",
      throw new RuntimeException("NODEPROF_HOME not set"),
    )
  }
  val JALANGI_HOME: String = {
    sys.env.getOrElse(
      "JALANGI_HOME",
      throw new RuntimeException("JALANGI_HOME not set"),
    )
  }
  val CHECK_SCRIPT_DIR: String = {
    val home = sys.env.getOrElse(
      "ESMETA_HOME",
      throw new RuntimeException("ESMETA_HOME not set"),
    )
    s"${home}/checkscript"
  }
  val ANALYSIS_FILE_PATH: String = {
    val home = sys.env.getOrElse(
      "ESMETA_HOME",
      throw new RuntimeException("ESMETA_HOME not set"),
    )
    val path = java.nio.file.Paths.get(
      home,
      if noCompare then "analysis.detail.js" else "analysis.compare.js",
    )
    if (!java.nio.file.Files.exists(path))
      throw new java.io.FileNotFoundException(s"${path} not found")
    path.toString
  }

  lazy val ANALYSIS_FILE_PATH_COPIED_TO_TMP: String = {
    val destPath = s"$TEMP_FILE_BASE/analysis.js"
    copyFile(ANALYSIS_FILE_PATH, destPath)
    destPath
  }

  object Aux {

    private val CHECK_SCRIPT_COMMAND: String = {
      val home = sys.env.getOrElse(
        "ESMETA_HOME",
        throw new RuntimeException("ESMETA_HOME not set"),
      )
      s"node ${home}/checkscript/dist/check.js"
    }

    /** Check syntax of a test file using acorn and some requirements. It should
      * be a valid ES5 syntax, not include `this`, `use strict`, etc.
      */
    def checkSyntax(filename: String): (Boolean, String, String) = {
      val (exitcode, str, err) = executeCmdNonZero(
        s"$CHECK_SCRIPT_COMMAND $filename",
      )
      // printlnIfSingle(s"ESLint output:\n${str.slice(0, 20)}...")
      (exitcode == 0, str, err)
    }

    val BOOTSTRAP_NODE_JS: String =
      s"${sys.env.getOrElse("ESMETA_HOME", throw new RuntimeException("ESMETA_HOME not set"))}/bootstrap-node.js"

    def runNodeJs(
      testpath: String,
    ): Boolean = {
      val (exitcode, _, _) = executeCmdNonZero(
        s"node --require $BOOTSTRAP_NODE_JS $testpath",
      )
      exitcode == 0
      // if it doesn't pass node.js, it is not worth to run Jalangi
    }

    /** @param wd
      *   absolute working directory
      * @param analysisPath
      *   absolute analysis file path
      * @param testPath
      *   absolute test file path
      * @return
      *   Jalangi output
      */
    def runNodeProf(
      analysisPath: String,
      testPath: String,
    ): (String, String) = {

      import java.nio.file.{Path, Paths, Files}

      // TODO make sure mx jalangi's boot.js has same content as BOOTSTRAP_NODE_JS

      val cmd =
        // s"docker run --rm -v $wd:/works/nodeprof.js/input nodeprof jalangi --analysis $relAnalysis $relTest"
        s"mx jalangi --analysis $analysisPath $testPath"
      val (str, err) =
        try {
          // printlnIfSingle(s"Executing Jalangi command: $cmd")
          val (s, err) =
            executeCmdTimeout(cmd, duration = 60.seconds, dir = NODEPROF_HOME)
              .getOrElse(
                throw java.util.concurrent.TimeoutException(
                  "NodeProf timed out, maybe because of ES6+ features?",
                ),
              )
          if (s.startsWith("Failed to instrument"))
            throw NotSupported(
              "The test may include new features not supported by NodeProf",
            )
          (s, err)
          // maybe test includes new features not supported by Jalangi
        } catch {
          case e: Throwable =>
            printlnIfSingle(
              s"Error in NodeProf Test262 Test: ${e.getStackTrace()}",
            )
            throw e;
        }
      (str, err)
    }

    def runESMetaInterp(
      code: String,
      ast: Ast,
      timeLimit: Option[Int],
    )(using test262: Test262): String = {
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

      output
    }

    def runJalangi(tmpFilePath: String): (String, String) = {
      val (str, err) =
        try {
          // printlnIfSingle(s"Executing Jalangi command: $jalangiCmd")
          val (s, err) =
            executeCmdTimeout(
              s"node --require ${BOOTSTRAP_NODE_JS} ${JALANGI_HOME}/src/js/commands/jalangi.js --inlineIID --inlineSource --analysis $ANALYSIS_FILE_PATH $tmpFilePath",
              duration = 60.seconds,
            ).getOrElse(
              throw java.util.concurrent.TimeoutException(
                "Jalangi timed out, maybe because of ES6+ features?",
              ),
            )
          if (s.startsWith("Failed to instrument"))
            throw NotSupported(
              "The test may include new features not supported by Jalangi",
            )
          (s, err)
          // maybe test includes new features not supported by Jalangi
        } catch {
          case e: Throwable =>
            printlnIfSingle(
              s"Error in Jalangi Test262 Test: ${e.getStackTrace()}",
            )
            throw e;
        }
      (str, err)
    }
  }
}
