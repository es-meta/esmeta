package esmeta.es.builtin

object TemplateReplacer {

  extension (model: Model)
    /** replace template name to instance name */
    def replace(templates: List[Template]): List[Model] = {
      val Model(name, tname, imap, nmap) = model
      templates.find(name contains _.name).fold(List(model)) { template =>
        val Template(from, instances) = template
        for ((to, fields) <- instances.toList)
          yield Model(
            name.replaceName(from, to),
            tname,
            imap.map { (k, v) => k -> v.replaceContent(from, to, fields) },
            nmap.map { (k, d) => k -> d.replaceContent(from, to, fields) },
          )
      }
    }

  extension (str: String)
    /** replace model name */
    def replaceName(from: String, to: String): String =
      str.replace(s"_${from}_", to)

    /** replace model content */
    def replaceContent(
      from: String,
      to: String,
      fields: Map[String, String],
    ): String = """\(\((.*?)\)\)""".r.replaceAllIn(
      str,
      _.group(1).trim.split('.').toList match
        case List(_, field) if fields.contains(field) => fields(field)
        case _                                        => to,
    )
}
