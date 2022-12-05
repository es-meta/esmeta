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
      case elem: Test => dataRule(app, elem)
    }

  // tests in Test262
  given dataRule: Rule[Test] = (app, data) =>
    val Test(path, neg, flags, includes, locales, features, es5) = data
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
}
