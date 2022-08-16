package esmeta

/** line seperator */
val LINE_SEP = System.getProperty("line.separator")

/** base project directory root */
val BASE_DIR = System.getenv("ESMETA_HOME")

/** log directory */
val LOG_DIR = s"$BASE_DIR/logs"
val EXTRACT_LOG_DIR = s"$LOG_DIR/extract"
val COMPILE_LOG_DIR = s"$LOG_DIR/compile"
val CFG_LOG_DIR = s"$LOG_DIR/cfg"
val ANALYZE_LOG_DIR = s"$LOG_DIR/analyze"
val INJECTOR_LOG_DIR = s"$LOG_DIR/injector"

/** tests directory root */
val TEST_DIR = s"$BASE_DIR/tests"

/** specification directory */
val ECMA262_DIR = s"$BASE_DIR/ecma262"
val SPEC_HTML = s"$ECMA262_DIR/spec.html"

/** current directory root */
val CUR_DIR = System.getProperty("user.dir")

/** source code directory */
val SRC_DIR = s"$BASE_DIR/src/main/scala/esmeta"

/** resource directory */
val RESOURCE_DIR = s"$BASE_DIR/src/main/resources"
val UNICODE_DIR = s"$RESOURCE_DIR/unicode"
val MANUALS_DIR = s"$RESOURCE_DIR/manuals"

/** package name */
val PACKAGE_NAME = "esmeta"

/** tests directory */
val IR_TEST_DIR = s"$TEST_DIR/ir"
val JS_TEST_DIR = s"$TEST_DIR/js"
val TEST262_DIR = s"$TEST_DIR/test262"
val TEST262_TEST_DIR = s"$TEST262_DIR/test"

// -----------------------------------------------------------------------------
// Mutable Global Options
// -----------------------------------------------------------------------------
/** debugging mode */
var DEBUG: Boolean = false

/** silent mode */
var SILENT: Boolean = false

/** show duration time */
var TIME: Boolean = false

/** test mode (turn it on only when executing tests) */
var TEST_MODE: Boolean = false
