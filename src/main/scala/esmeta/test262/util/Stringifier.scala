package esmeta.test262.util

import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.test262.*

/*k stringifier for ECMAScript */
class Stringifier(
  detail: Boolean,
) {
  // elements
  given elemRule: Rule[Test262Elem] = (app, elem) =>
    elem match {
      case elem: MetaData      => dataRule(app, elem)
      case elem: ConfigSummary => summaryRule(app, elem)
      case elem: NormalConfig  => normalConfigRule(app, elem)
      case elem: ErrorConfig   => errorConfigRule(app, elem)
    }

  // metadata tests in Test262
  given dataRule: Rule[MetaData] = (app, data) =>
    val MetaData(_, path, neg, flags, includes, locales, features, es5) = data
    given Rule[Option[String]] = optionRule("-")
    given Rule[List[String]] =
      (app, list) => app.wrap("[", "]")(list.map(app :> _))
    app >> path >> " : "
    app.wrap {
      if (!neg.isEmpty) app :> "- negative: " >> neg
      if (!flags.isEmpty) app :> "- flags: " >> flags
      if (!includes.isEmpty) app :> "- includes: " >> includes
      if (!features.isEmpty) app :> "- features: " >> features
      app :> "- es5: " >> es5
    }

  // test configuration summary
  given summaryRule: Rule[ConfigSummary] = (app, summary) => {
    val ConfigSummary(data, normal, error) = summary
    app >> "total " >> data.length >> " tests:"
    app.wrap("", "") {
      app :> "- positive tests: " >> normal.length
      if (detail) app.wrap("", "") { normal.map(app :> _) }
      app :> "- negative tests: " >> error.length
      if (detail) app.wrap("", "") { error.map(app :> _) }
    }
  }

  // configuration for normal tests
  given normalConfigRule: Rule[NormalConfig] = (app, config) => {
    val NormalConfig(name, includes) = config
    given Rule[List[String]] =
      (app, list) => app.wrap("[", "]")(list.map(app :> _))
    app >> name >> " : "
    app.wrap { app :> "- includes: " >> includes }
    app
  }

  // configuration for error tests
  given errorConfigRule: Rule[ErrorConfig] = (app, config) => {
    val ErrorConfig(name, errorName, includes) = config
    given Rule[List[String]] =
      (app, list) => app.wrap("[", "]")(list.map(app :> _))
    app >> name >> " : "
    app.wrap {
      app :> "- errorName: " >> errorName
      if (!includes.isEmpty) app :> "- includes: " >> includes
    }
    app
  }
}
