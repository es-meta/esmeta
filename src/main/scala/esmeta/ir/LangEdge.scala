package esmeta.ir

import esmeta.lang.Syntax
import esmeta.util.Loc

/** backward edge to metalangauge */
trait LangEdge:
  /** backward edge to metalangauge */
  var langOpt: Option[Syntax] = None

  /** update backward edge to metalangauge */
  def setLang(lang: Syntax): this.type = setLangOpt(Some(lang))

  /** update backward edge to metalangauge */
  def setLangOpt(langOpt: Option[Syntax]): this.type =
    this.langOpt = langOpt; this

  /** get source locations */
  def loc: Option[Loc] = langOpt.flatMap(_.loc)
