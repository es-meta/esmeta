package esmeta.es.util.dsl

import esmeta.es.util.dsl.AstExtensions.flatten
import esmeta.es.util.dsl.AstExtensions.rawSteps
import esmeta.lang.*
import esmeta.lang.util.UnitWalker as LangUnitWalker
import esmeta.lang.util.Walker as LangWalker
import esmeta.util.SystemUtils.*
import io.circe.*
import io.circe.yaml.scalayaml.parser as yamlParser

import java.io.File
import scala.collection.mutable
import scala.util.Try

object DSLRuleParser {

  def parseDir(dir: String): List[Rule[LangElem]] = {
    // println(s"[DSLRuleParser] parseDir: $dir")
    val files = new File(dir)
      .listFiles()
      .filter(_.getName.endsWith(".yaml"))
      .sortBy(_.getName)
    // println(s"[DSLRuleParser] found ${files.length} YAML files: ${files.map(_.getName).mkString(", ")}")
    val rules = files.flatMap(f => parseFile(f.getAbsolutePath)).toList
    // println(s"[DSLRuleParser] parsed ${rules.length} rules total")
    rules
  }

  def parseFile(path: String): List[Rule[LangElem]] = {
    // println(s"[DSLRuleParser] parseFile: $path")
    val content = readFile(path)

    val json = yamlParser.parse(content) match {
      case Right(j) => j
      case Left(e)  =>
        // println(s"[DSLRuleParser]   YAML parse error: $e")
        throw new RuntimeException(s"YAML syntax error in $path: $e")
    }

    json.asObject match {
      case Some(_) =>
        try {
          val rule = parseRule(json)
          // println(s"[DSLRuleParser]   OK: ${rule.name} (${rule.getClass.getSimpleName})")
          List(rule)
        } catch {
          case e: Exception =>
            // println(s"[DSLRuleParser]   FAILED: ${e.getMessage}")
            throw e
        }
      case None =>
        throw new RuntimeException(
          s"Expected YAML array or object in $path",
        )
    }
  }

  // No yet
  // No blockstep with single step
  def ensureComplete[T <: LangElem](body: T): T = {
    var yetFound = false
    var hasSingleStepBlock = false

    new LangUnitWalker {
      override def walk(step: Step): Unit = step match
        case BlockStep(block) =>
          hasSingleStepBlock ||= block.rawSteps.length == 1
        case YetStep(expr) =>
          println("YET: " + expr)
          yetFound = true
        case _ => super.walk(step)
      override def walk(expr: Expression): Unit = expr match
        case YetExpression(str, block) =>
          println("YET: " + str)
          yetFound = true
        case _ => super.walk(expr)
    }.walk(body)

    if (yetFound) throw Exception("YET")
    if (hasSingleStepBlock) throw Exception("Single Step Block")

    body
  }

  def metaDefs(body: LangElem): Map[String, LangElem] = {
    val result = mutable.Map[String, LangElem]()
    new LangUnitWalker {
      override def walk(step: Step): Unit = step match
        case MetaStep(name, _, _) => result += (name -> step)
        case _                    => super.walk(step)
      override def walk(expr: Expression): Unit = expr match
        case MetaExpression(name, _) => result += (name -> expr)
        case _                       => super.walk(expr)
      override def walk(cond: Condition): Unit = cond match
        case MetaCondition(name, _) => result += (name -> cond)
        case _                      => super.walk(cond)
      override def walk(x: Variable): Unit = x match
        case v @ Variable(name, _, true, _) =>
          result += (name -> v)
        case _ => super.walk(x)
      override def walk(ref: Reference): Unit = ref match
        case MetaReference(name, _) => result += (name -> ref)
        case _                      => super.walk(ref)
    }.walk(body)

    result.toMap
  }

  def addIndent(str: String): String =
    str.split("\n").mkString("\n  ", "\n  ", "")

  /** Parse a single rule JSON object. */
  def parseRule(
    json: Json,
    defs: Map[String, LangElem] = Map.empty,
  ): Rule[LangElem] = {
    val obj = json.asObject.getOrElse(
      throw new RuntimeException(s"Expected object: $json"),
    )

    val name = getString(obj, "name")
    val patternText = getString(obj, "pattern").trim
    val replaceText =
      getStringOpt(obj, "replace").map(_.trim).filter(_.nonEmpty)
    val predicates = parsePredicateConstraints(obj)

    // println(s"[DSLRuleParser]   parseRule '$name'")
    // println(s"[DSLRuleParser]     pattern: ${patternText.take(80)}")
    // println(s"[DSLRuleParser]     replace: ${replaceText.map(_.take(80))}")
    // println(s"[DSLRuleParser]     predicates: ${predicates.keys.mkString(", ")}")

    // Auto-detect pattern type: try ref, cond, expr, step in order
    val attempts = List[(String, String => LangElem)](
      "ref" -> (text => ensureComplete(DSLParser(defs).parseRef(text))),
      "expr" -> (text => ensureComplete(DSLParser(defs).parseExpr(text))),
      "cond" -> (text => ensureComplete(DSLParser(defs).parseCond(text))),
      "step" -> (text =>
        ensureComplete(DSLParser(defs).parseStep(addIndent(text)).flatten),
      ),
    )
    val results = attempts.map {
      case (label, f) =>
        val r = label -> Try(f(patternText))
        r._2 match
          case scala.util.Success(
                _,
              ) => // println(s"[DSLRuleParser]     try $label: OK")
          case scala.util.Failure(
                e,
              ) => // println(s"[DSLRuleParser]     try $label: ${e.getMessage.take(60)}")
        r
    }
    val patternElem: LangElem = results
      .collectFirst {
        case (_, scala.util.Success(elem)) =>
          elem
      }
      .getOrElse {
        val errors = results
          .map { case (l, r) => s"  $l: ${r.failed.get.getMessage}" }
          .mkString("\n")
        throw new RuntimeException(
          s"Rule '$name': failed to parse pattern:\n$errors\n--- pattern ---\n$patternText",
        )
      }

    val patternDefs = metaDefs(patternElem) ++ defs

    // Parse subrules with inherited meta-variable definitions
    val subrules = getArrayOpt(obj, "subrules")
      .map(_.toList.map(subrule => parseRule(subrule, patternDefs)))
      .getOrElse(List.empty)

    // Construct the appropriate rule type based on parsed pattern
    patternElem match {
      case patRef: Reference =>
        val repRef = replaceText.map(rt =>
          ensureComplete(DSLParser(patternDefs).parseRef(rt)),
        )
        ReferenceRule(name, patRef, repRef, predicates, subrules)
      case patCond: Condition =>
        val repCond =
          replaceText.map(rt =>
            ensureComplete(DSLParser(patternDefs).parseCond(rt)),
          )
        ConditionRule(name, patCond, repCond, predicates, subrules)
      case patExpr: Expression =>
        val repExpr =
          replaceText.map(rt =>
            ensureComplete(DSLParser(patternDefs).parseExpr(rt)),
          )
        ExpressionRule(name, patExpr, repExpr, predicates, subrules)
      case patStep: Step =>
        // println(s"[DSLRuleParser]     patternDefs: ${patternDefs.map { case (k, v) => s"$k:${v.getClass.getSimpleName}" }.mkString(", ")}")
        val repStep =
          replaceText.map { rt =>
            ensureComplete(
              DSLParser(patternDefs).parseStep(addIndent(rt)).flatten,
            )
          }
        StepRule(name, patStep, repStep, predicates, subrules)
      case other =>
        throw new RuntimeException(
          s"Rule '$name': unexpected pattern type: ${other.getClass.getSimpleName}",
        )
    }
  }

  // ---------------------------------------------------------------------------
  // Predicate constraint parsing
  // ---------------------------------------------------------------------------

  private def parsePredicateConstraints(
    obj: JsonObject,
  ): Map[String, LangElemPredicate] = {
    obj("where") match {
      case None => Map.empty
      case Some(whereJson) =>
        val (applications, definitions) = whereJson.asArray match {
          case Some(arr) =>
            val apps = arr.flatMap(_.asString).toList
            val defs = arr
              .flatMap(_.asObject)
              .flatMap(_.toList.collect {
                case (name, value) if value.asString.isDefined =>
                  name -> value.asString.get
              })
              .toMap
            (apps, defs)
          case None => (Nil, Map.empty)
        }

        applications
          .flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
          .map { entry =>
            val pattern =
              "([a-zA-Z_][a-zA-Z0-9_]*)\\(([a-zA-Z_][a-zA-Z0-9_]*)\\)".r
            entry match {
              case pattern(predName, varName) =>
                val predExpr = definitions.getOrElse(
                  predName,
                  throw new RuntimeException(
                    s"Predicate '$predName' not defined in where list",
                  ),
                )
                val regex = PredicateExpr.parse(predExpr)
                val pred: LangElemPredicate = (elem, ctx) =>
                  elem match {
                    case ref: Reference =>
                      val path = ctx.symbolicPaths
                        .getOrElse(
                          varName,
                          Analyzer.resolvePath(ref, ctx.symbolicPaths),
                        )
                      val result =
                        path.nonEmpty && PredicateExpr.matches(path, regex)
                      // println(s"    [PRED-DETAIL] pred=$predName ref=$ref path=${path.mkString(".")} result=$result")
                      result
                    case other =>
                      // println(s"    [PRED-DETAIL] unhandled type=${other.getClass.getSimpleName} node=$other")
                      false
                  }
                varName -> pred
              case _ =>
                throw new RuntimeException(
                  s"Invalid where constraint: '$entry'. Expected format: predName(varName)",
                )
            }
          }
          .toMap
    }
  }

  /** Parse the legacy predicates: YAML list into a map of name → expression
    * string.
    */
  private def parsePredDefinitions(
    obj: JsonObject,
  ): Map[String, String] = {
    getArrayOpt(obj, "predicates")
      .map { arr =>
        arr.flatMap { json =>
          json.asObject
            .map { inner =>
              inner.toList.collect {
                case (name, value) if value.asString.isDefined =>
                  name -> value.asString.get
              }
            }
            .getOrElse(List.empty)
        }.toMap
      }
      .getOrElse(Map.empty)
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
      DSLParser().parseStep(t)
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
    val parsed = DSLParser().parseStep(wrapped)
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
    try { Some(DSLParser().parseExpr(text)) }
    catch { case _: Throwable => None }
  }

  // ---------------------------------------------------------------------------
  // JSON helpers
  // ---------------------------------------------------------------------------

  private def getString(obj: JsonObject, key: String): String =
    obj(key)
      .flatMap(_.asString)
      .getOrElse(throw new RuntimeException(s"Missing string field '$key'"))
      .strip()

  private def getStringOpt(
    obj: JsonObject,
    key: String,
  ): Option[String] =
    obj(key).flatMap(_.asString).map(_.strip())

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
