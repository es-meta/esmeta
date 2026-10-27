package esmeta.es.util.polyfill.dsl

import AstExtensions.flatten
import AstExtensions.rawSteps
import esmeta.lang.*
import esmeta.lang.util.UnitWalker as LangUnitWalker
import esmeta.util.SystemUtils.*
import io.circe.*
import io.circe.yaml.scalayaml.parser as yamlParser

import java.io.File
import scala.collection.mutable
import scala.util.Try

object DSLRuleParser {

  def parseDir(dir: String): List[Rule[LangElem]] =
    new File(dir)
      .listFiles()
      .filter(_.getName.endsWith(".yaml"))
      .sortBy(_.getName)
      .flatMap(f => parseFile(f.getAbsolutePath))
      .toList

  def parseFile(path: String): List[Rule[LangElem]] = {
    val json = yamlParser.parse(readFile(path)) match {
      case Right(j) => j
      case Left(e) =>
        throw new RuntimeException(s"YAML syntax error in $path: $e")
    }

    json.asObject match {
      case Some(_) => List(parseRule(json))
      case None =>
        throw new RuntimeException(
          s"Expected YAML array or object in $path",
        )
    }
  }

  def ensureComplete[T <: LangElem](body: T): T = {
    var yetFound = false
    var hasSingleStepBlock = false

    new LangUnitWalker {
      override def walk(step: Step): Unit = step match
        case BlockStep(block) =>
          hasSingleStepBlock ||= block.rawSteps.length == 1
        case YetStep(expr) =>
          yetFound = true
        case _ => super.walk(step)
      override def walk(expr: Expression): Unit = expr match
        case YetExpression(str, block) =>
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

    // the pattern's syntactic category is not declared, so each is tried in turn
    val attempts = List[(String, String => LangElem)](
      "ref" -> (text => ensureComplete(DSLParser(defs).parseRef(text))),
      "expr" -> (text => ensureComplete(DSLParser(defs).parseExpr(text))),
      "cond" -> (text => ensureComplete(DSLParser(defs).parseCond(text))),
      "step" -> (text =>
        ensureComplete(DSLParser(defs).parseStep(addIndent(text)).flatten),
      ),
    )
    val results = attempts.map {
      case (label, f) => label -> Try(f(patternText))
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

    val subrules = getArrayOpt(obj, "subrules")
      .map(_.toList.map(subrule => parseRule(subrule, patternDefs)))
      .getOrElse(List.empty)

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
                      path.nonEmpty && PredicateExpr.matches(path, regex)
                    case _ => false
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

  private def getArrayOpt(
    obj: JsonObject,
    key: String,
  ): Option[Vector[Json]] =
    obj(key).flatMap(_.asArray)
}
