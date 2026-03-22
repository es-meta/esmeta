package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.util.SystemUtils.*
import io.circe.*
import io.circe.yaml.scalayaml.parser as yamlParser

import java.io.File

/** Parses DSL rule files (.yaml) into Rule objects. */
object DSLRuleParser {

  /** Parse all .yaml files in a directory. */
  def parseDir(dir: String): List[Rule] = {
    val files = new File(dir)
      .listFiles()
      .filter(_.getName.endsWith(".yaml"))
      .sortBy(_.getName)
    files.flatMap(f => parseFile(f.getAbsolutePath)).toList
  }

  /** Parse a single .yaml file into a list of Rules. */
  def parseFile(path: String): List[Rule] = {
    val content = readFile(path)
    val json = yamlParser.parse(content) match {
      case Right(j) => j
      case Left(e) =>
        throw new RuntimeException(s"YAML parse error in $path: $e")
    }
    json.asArray match {
      case Some(arr) => arr.toList.map(parseRule)
      case None      =>
        // Single rule object (individual file)
        json.asObject match {
          case Some(_) => List(parseRule(json))
          case None =>
            throw new RuntimeException(
              s"Expected YAML array or object in $path",
            )
        }
    }
  }

  /** Parse a single rule JSON object. */
  def parseRule(json: Json): Rule = {
    val obj = json.asObject.getOrElse(
      throw new RuntimeException(s"Expected object: $json"),
    )
    val name = getString(obj, "name")
    val ruleType = getStringOpt(obj, "type")
    val patternText = getStringOpt(obj, "pattern")
    val replaceText = getStringOpt(obj, "replace")
    val whereText = getStringOpt(obj, "where")
    val predicateText = getStringOpt(obj, "predicate")
    val isDelete = getBoolOpt(obj, "delete").getOrElse(false)
    val subrules = getArrayOpt(obj, "subrules")
      .map(_.toList.map(parseRule))
      .getOrElse(List.empty)

    val predicates = predicateText
      .map(parsePredicate)
      .getOrElse(Map.empty)

    // If delete: true, replace is None
    val effectiveReplace =
      if (isDelete) None else replaceText

    // Parse closureConfig if present
    val closureConfig = obj("closureConfig").flatMap(_.asObject).map { cc =>
      ClosureConfig(
        aoName = cc("aoName").flatMap(_.asString).getOrElse(""),
        iterBase = cc("iterBase").flatMap(_.asString).getOrElse(""),
        elementVar = cc("elementVar").flatMap(_.asString).getOrElse(""),
        bodyHole = cc("bodyHole").flatMap(_.asString).getOrElse("$body"),
        earlyReturn = cc("earlyReturn").flatMap(_.asBoolean).getOrElse(false),
      )
    }

    // Parse copyCheck if present
    val copyCheck = obj("copyCheck").flatMap(_.asArray).map { arr =>
      val items = arr.flatMap(_.asString)
      (items(0), items(1))
    }

    // Handle type: reference explicitly
    if (ruleType.contains("reference")) {
      val pt = patternText.getOrElse(
        throw new RuntimeException(
          s"Rule '$name': reference rule needs 'pattern'",
        ),
      )
      val rt = replaceText.getOrElse(
        throw new RuntimeException(
          s"Rule '$name': reference rule needs 'replace'",
        ),
      )
      return ReferenceRule(
        name = name,
        pattern = DSLParser.parseRef(pt.trim),
        replace = DSLParser.parseRef(rt.trim),
        predicates = predicates,
      )
    }

    whereText match {
      case Some(wt) =>
        // WhereRule: where + optional pattern/replace as mainRules
        val whereStep = parseStepText(wt.trim)
        val mainRules = patternText match {
          case Some(pt) =>
            subrules ++ parsePatternReplace(
              name,
              pt,
              effectiveReplace,
              predicates,
              List.empty,
            )
          case None =>
            // where-only: subrules are the mainRules
            subrules
        }
        WhereRule(
          name = name,
          wherePattern = whereStep,
          mainRules = mainRules,
          predicates = predicates,
        )
      case None =>
        patternText match {
          case Some(pt) =>
            parsePatternReplace(
              name,
              pt,
              effectiveReplace,
              predicates,
              subrules,
              closureConfig,
              copyCheck,
            ) match {
              case List(single) => single
              case rules =>
                throw new RuntimeException(
                  s"Rule '$name' produced ${rules.length} rules, expected 1",
                )
            }
          case None =>
            throw new RuntimeException(
              s"Rule '$name': needs 'pattern' or 'where' field",
            )
        }
    }
  }

  // ---------------------------------------------------------------------------
  // Determine rule type from pattern text
  // ---------------------------------------------------------------------------

  /** Parse pattern/replace into the appropriate Rule type. */
  private def parsePatternReplace(
    name: String,
    patternText: String,
    replaceText: Option[String],
    predicates: Map[String, LangElemPredicate],
    subrules: List[Rule],
    closureConfig: Option[ClosureConfig] = None,
    copyCheck: Option[(String, String)] = None,
  ): List[Rule] = {
    val pt = patternText.trim

    // Try as step (starts with "1." or is a known step keyword)
    if (pt.startsWith("1.")) {
      val patternSteps = parseStepListText(pt)
      val replaceSteps =
        replaceText.map(rt => parseStepListText(rt.trim))

      if (patternSteps.length == 1 && closureConfig.isEmpty) {
        // Single step → StepRule
        List(
          StepRule(
            name = name,
            pattern = patternSteps.head,
            replace = replaceSteps.map(_.head),
            predicates = predicates,
            subrules = subrules,
          ),
        )
      } else {
        // Multi-step → StepBlockRule
        List(
          StepBlockRule(
            name = name,
            patternSteps = patternSteps,
            replace = replaceSteps.getOrElse(List.empty),
            predicates = predicates,
            subrules = subrules,
            closureConfig = closureConfig,
            copyCheck = copyCheck,
          ),
        )
      }
    } else {
      // Try as expression first, then condition
      tryParseExpr(pt) match {
        case Some(patExpr) =>
          val repExpr = replaceText
            .map(rt => DSLParser.parseExpr(rt.trim))
            .getOrElse(
              throw new RuntimeException(
                s"Rule '$name': expression rule needs replace",
              ),
            )
          List(
            ExpressionRule(
              name = name,
              pattern = patExpr,
              replace = repExpr,
              predicates = predicates,
            ),
          )
        case None =>
          // Try as condition
          val patCond = DSLParser.parseCond(pt)
          val repCond = replaceText
            .map(rt => DSLParser.parseCond(rt.trim))
            .getOrElse(
              throw new RuntimeException(
                s"Rule '$name': condition rule needs replace",
              ),
            )
          List(
            ConditionRule(
              name = name,
              pattern = patCond,
              replace = repCond,
              predicates = predicates,
            ),
          )
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Text parsing helpers
  // ---------------------------------------------------------------------------

  /** Parse text as a single step. */
  private def parseStepText(text: String): Step = {
    val t = text.trim
    if (t.startsWith("1.")) {
      val steps = parseStepListText(t)
      if (steps.length == 1) steps.head
      else BlockStep(StepBlock(steps.map(SubStep(None, _))))
    } else {
      DSLParser.parseStep(t)
    }
  }

  /** Parse text as a list of steps (numbered "1. ..."). Wraps in a dummy
    * ForEach to get the block parser to work, then extracts the step list.
    */
  private def parseStepListText(text: String): List[Step] = {
    val trimmed = text.trim
    // Wrap in a dummy step so the indent parser can handle the block
    val wrapped =
      s"for each _dummy_ of _dummy_, do\n${trimmed.linesIterator.map("  " + _).mkString("\n")}"
    val parsed = DSLParser.parseStep(wrapped)
    parsed match {
      case ForEachStep(_, _, _, _, BlockStep(StepBlock(steps))) =>
        steps.map(_.step)
      case ForEachStep(_, _, _, _, singleStep) =>
        List(singleStep)
      case _ =>
        throw new RuntimeException(
          s"Failed to parse step list: $trimmed",
        )
    }
  }

  /** Try to parse text as an expression. */
  private def tryParseExpr(text: String): Option[Expression] = {
    try { Some(DSLParser.parseExpr(text)) }
    catch { case _: Throwable => None }
  }

  /** Try to parse text as a condition. */
  private def tryParseCond(text: String): Option[Condition] = {
    try { Some(DSLParser.parseCond(text)) }
    catch { case _: Throwable => None }
  }

  // ---------------------------------------------------------------------------
  // Predicate parsing
  // ---------------------------------------------------------------------------

  /** Parse "$ ref -> isSetData" into predicate map. */
  private def parsePredicate(
    text: String,
  ): Map[String, LangElemPredicate] = {
    text
      .split(",")
      .map(_.trim)
      .filter(_.nonEmpty)
      .map { entry =>
        val parts = entry.split("->").map(_.trim)
        if (parts.length != 2)
          throw new RuntimeException(s"Invalid predicate: '$entry'")
        val varName = parts(0)
        val predName = parts(1)
        varName -> PredicateRegistry(predName)
      }
      .toMap
  }

  // ---------------------------------------------------------------------------
  // JSON helpers
  // ---------------------------------------------------------------------------

  private def getString(obj: JsonObject, key: String): String =
    obj(key)
      .flatMap(_.asString)
      .getOrElse(throw new RuntimeException(s"Missing string field '$key'"))

  private def getStringOpt(
    obj: JsonObject,
    key: String,
  ): Option[String] =
    obj(key).flatMap(_.asString)

  private def getBoolOpt(
    obj: JsonObject,
    key: String,
  ): Option[Boolean] =
    obj(key).flatMap(_.asBoolean)

  private def getArrayOpt(
    obj: JsonObject,
    key: String,
  ): Option[Vector[Json]] =
    obj(key).flatMap(_.asArray)
}

/** Registry of named predicates referenced from YAML files. */
object PredicateRegistry {
  private val registry: Map[String, LangElemPredicate] = Map(
    "isSetData" -> ((elem, ctx) =>
      elem match {
        case Access(_, "SetData", _, _) => true
        case Variable(v, _) =>
          ctx.variableTypes.get(v).contains("SetData")
        case _ => false
      },
    ),
    "isMapData" -> ((elem, ctx) =>
      elem match {
        case Access(_, "MapData", _, _) => true
        case Variable(v, _) =>
          ctx.variableTypes.get(v).contains("MapData")
        case _ => false
      },
    ),
    "isSameOrCopyOf" -> ((elem, ctx) =>
      elem match {
        case Variable(v, _) =>
          ctx.variableTypes.get(v).contains("SetData") ||
          ctx.variableTypes.get(v).contains("MapData") ||
          ctx.copyOf.contains(v)
        case Access(_, "SetData", _, _) => true
        case Access(_, "MapData", _, _) => true
        case _                          => false
      },
    ),
  )

  def apply(name: String): LangElemPredicate =
    registry.getOrElse(
      name,
      throw new RuntimeException(s"Unknown predicate: '$name'"),
    )

  /** Reverse-lookup: find the name for a predicate function. */
  def reverseLookup(pred: LangElemPredicate): Option[String] =
    registry.collectFirst { case (name, p) if p eq pred => name }
}
