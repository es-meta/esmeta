package esmeta.extractor

import esmeta.lang.util.Parser
import esmeta.spec.{Dfn, Table}
import esmeta.ty.TyDecl
import esmeta.ty.TyDecl.Elem.Field

/** type model extraction test */
class TyModelTinyTest extends ExtractorTest {
  val name: String = "extractorTyModelTest"

  // registration
  def init: Unit = {
    def table(
      id: String,
      caption: String,
      header: List[String],
      rows: List[List[String]],
    ): (String, Table) = id -> Table(id, caption, header, rows)

    val tables = Map(
      // record fields with `Value` column
      table(
        "t1",
        "Completion Record Fields",
        List("Field Name", "Value", "Meaning"),
        List(
          List("[[Type]]", "~normal~, ~break~, or ~throw~", "..."),
          List("[[Value]]", "any value except a Completion Record", "..."),
        ),
      ),
      // additional fields with `Value Type` column and hyphenated name
      table(
        "t2",
        "Additional Fields of For-In Iterator Records",
        List("Field Name", "Value Type", "Meaning"),
        List(List("[[Done]]", "a Boolean", "...")),
      ),
      // internal slots: `Instances` are dropped and `Type` column is used
      table(
        "t3",
        "Internal Slots of Promise Instances",
        List("Internal Slot", "Type", "Description"),
        List(List("[[PromiseIsHandled]]", "a Boolean", "...")),
      ),
      // value column is not the second one
      table(
        "t4",
        "PrivateElement Fields",
        List("Field Name", "Values of the [[Kind]] field", "Value", "Meaning"),
        List(List("[[Key]]", "All", "a String", "...")),
      ),
      // canonicalized by glossary definitions (modulo `Record`/`Event`)
      table(
        "t5",
        "AsyncGeneratorRequest Record Fields",
        List("Field Name", "Value", "Meaning"),
        List(List("[[Completion]]", "a Completion Record", "...")),
      ),
      table(
        "t6",
        "WriteSharedMemory Event Fields",
        List("Field Name", "Value", "Meaning"),
        List(List("[[NoTear]]", "a Boolean", "...")),
      ),
      // lowercase definitions are not names
      table(
        "t7",
        "Candidate Execution Record Fields",
        List("Field Name", "Value", "Meaning"),
        List(List("[[ChosenValues]]", "a List of Chosen Value Records", "...")),
      ),
      // not a field table
      table(
        "t8",
        "Module fields after module _A_ finishes executing",
        List("Field Module", "_A_", "[[Status]]"),
        List(List("_A_", "~evaluated~", "...")),
      ),
      // a field table without a valid type name
      table(
        "t9",
        "Something Else",
        List("Field Name", "Value", "Meaning"),
        List(List("[[X]]", "a Boolean", "...")),
      ),
    )

    val dfns = List(
      Dfn("Completion Record", List("Completion Records")),
      Dfn("AsyncGeneratorRequest", List("AsyncGeneratorRequests")),
      Dfn("WriteSharedMemory"),
      Dfn("candidate execution", List("candidate executions")),
    )

    val extractor = TyModelExtractor(tables, dfns, Parser)

    checkEqual("decls")(
      extractor.decls -> List(
        TyDecl(
          "CompletionRecord",
          None,
          List(
            Field("Type", false, "Enum[~break~, ~normal~, ~throw~]"),
            Field("Value", false, "Any"),
          ),
        ),
        TyDecl("ForInIteratorRecord", None, List(Field("Done", false, "Boolean"))),
        TyDecl("Promise", None, List(Field("PromiseIsHandled", false, "Boolean"))),
        TyDecl("PrivateElement", None, List(Field("Key", false, "String"))),
        TyDecl(
          "AsyncGeneratorRequest",
          None,
          List(Field("Completion", false, "Completion")),
        ),
        TyDecl("WriteSharedMemory", None, List(Field("NoTear", false, "Boolean"))),
        TyDecl(
          "CandidateExecutionRecord",
          None,
          List(Field("ChosenValues", false, "List[Record[ChosenValueRecord]]")),
        ),
      ),
    )
  }

  init
}
