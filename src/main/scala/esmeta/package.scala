package esmeta

/** line seperator */
val LINE_SEP = System.getProperty("line.separator")

/** base project directory root */
val BASE_DIR = System.getenv("ESMETA_HOME")

/** log directory */
val LOG_DIR = s"$BASE_DIR/logs"

/** tests directory root */
val TEST_DIR = s"$BASE_DIR/tests"

/** current directory root */
val CUR_DIR = System.getProperty("user.dir")

/** source code directory */
val SRC_DIR = s"$BASE_DIR/src/main/scala/esmeta"

/** resource directory */
val RESOURCE_DIR = s"$BASE_DIR/src/main/resources"

/** package name */
val PACKAGE_NAME = "esmeta"

/** tests directory */
val IR_TEST_DIR = s"$TEST_DIR/ir"

////////////////////////////////////////////////////////////////////////////////
// Mutable Global Options
////////////////////////////////////////////////////////////////////////////////
/** debugging mode */
var DEBUG: Boolean = false

/** silent mode */
var SILENT: Boolean = false

/** show duration time */
var TIME: Boolean = false

/** logging mode */
var LOG: Boolean = false

/** test mode (turn it on only when executing tests) */
var TEST_MODE: Boolean = false
