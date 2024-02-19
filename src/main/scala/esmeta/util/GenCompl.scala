package esmeta.util

import esmeta.*
import SystemUtils.*

// autocompltion fil generator for shell
object GenCompl {
  import Appender.*

  // the main method
  def main(args: Array[String]): Unit = update

  // generate completion file "$ESMETA_HOME/.completion"
  def update: Unit = dumpFile(content, path)

  // commands
  val commands = ESMeta.commands

  // phases
  val phases = ESMeta.phases

  // global options
  val options = ESMeta.options

  // get content
  def content: String = {
    val app = Appender()
    app >> """# `complete` for zsh
    |if type complete &>/dev/null; then
    |  :
    |else
    |  autoload bashcompinit
    |  bashcompinit
    |fi
    |
    |# completion for esmeta
    |_esmeta_completions() """.stripMargin
    app.wrap {
      app :> "local cur prev opts lastc informats outformats datafiles"
      app :> "cur=\"${COMP_WORDS[COMP_CWORD]}\""
      app :> "prev=\"${COMP_WORDS[COMP_CWORD-1]}\""

      // commands
      app :> "cmdList=\"" >> commands.map(_.name) >> "\""

      // global options
      app :> "globalOpt=\"" >> options.map("-" + _._1) >> "\""

      // phase options
      for (phase <- phases) {
        val name = phase.name
        val opts = phase.options.map("-" + name + ":" + _._1)
        app :> norm(name) >> "Opt=\"" >> opts >> "\""
      }

      // completion for commands
      app :> "# completion for commands"
      app :> "case \"${COMP_CWORD}\" in"
      app :> "  1)"
      app :> "    COMPREPLY=($(compgen -W \"version -version --version ${cmdList}\"))"
      app :> "    return 0"
      app :> "    ;;"
      app :> "esac"
      app :> "cmd=\"${COMP_WORDS[1]}\""

      // completion for options
      app :> "# completion for options"
      (app :> "").wrap("case \"${cur}\" in", "esac") {
        (app :> "").wrap("-*)", "") {
          (app :> "").wrap("case \"${cmd}\" in", "esac") {
            for (cmd <- commands) (app :> "").wrap(cmd.name + ")", "") {
              app :> "COMPREPLY=($(compgen -W "
              val names = cmd.pList.names.map(s => "${" + norm(s) + "Opt}")
              app >> "\"${globalOpt} " >> names >> "\"))"
              app :> "return 0"
              app :> ";;"
            }
          }
          app :> "return 0"
          app :> ";;"
        }
      }
      app :> "COMPREPLY=( $(compgen -X '' -f \"${cur}\") )"
    }

    app :> "# completion setting"
    app :> "complete -o filenames -o bashdefault -F _esmeta_completions esmeta"

    app.toString
  }

  // path of completion file
  val path = s"$BASE_DIR/.completion"

  // normalize name
  def norm(name: String): String = name.replaceAll("-", "")

  // appender rule for string iterable
  given Rule[Iterable[String]] = iterableRule(sep = " ")
}
