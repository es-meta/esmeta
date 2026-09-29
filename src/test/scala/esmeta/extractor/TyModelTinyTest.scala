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
    // field tables in the style of ECMA-262
    val FIELDS = List("Field Name", "Value", "Meaning")
    val FIELDS_TY = List("Field Name", "Value Type", "Meaning")
    val SLOTS = List("Internal Slot", "Type", "Description")
    val tables = List(
      Table(
        "table-completion-record-fields",
        "Completion Record Fields",
        FIELDS,
        List(
          List("[[Type]]", "~normal~, ~break~, or ~throw~", "..."),
          List("[[Value]]", "any value except a Completion Record", "..."),
          List("[[Target]]", "a String or ~empty~", "..."),
        ),
      ),
      Table(
        "table-privateelement-fields",
        "PrivateElement Fields",
        List("Field Name", "Values of the [[Kind]] field", "Value", "Meaning"),
        List(
          List("[[Key]]", "All", "a Private Name", "..."),
          List(
            "[[Get]]",
            "~accessor~",
            "a function object or *undefined*",
            "...",
          ),
        ),
      ),
      Table(
        "table-additional-fields-of-cyclic-module-records",
        "Additional Fields of Cyclic Module Records",
        FIELDS_TY,
        List(List("[[HasTLA]]", "a Boolean", "...")),
      ),
      Table(
        "table-async-from-sync-iterator-internal-slots",
        "Internal Slots of Async-from-Sync Iterator Instances",
        SLOTS,
        List(List("[[SyncIteratorRecord]]", "an Iterator Record", "...")),
      ),
      Table(
        "table-internal-slots-of-bound-function-exotic-objects",
        "Internal Slots of Bound Function Exotic Objects",
        SLOTS,
        List(
          List(
            "[[BoundArguments]]",
            "a List of ECMAScript language values",
            "...",
          ),
        ),
      ),
      Table(
        "sec-asyncgeneratorrequest-records",
        "AsyncGeneratorRequest Record Fields",
        FIELDS,
        List(List("[[Completion]]", "a Completion Record", "...")),
      ),
      Table(
        "table-writesharedmemory-fields",
        "WriteSharedMemory Event Fields",
        FIELDS,
        List(List("[[NoTear]]", "a Boolean", "...")),
      ),
      Table(
        "table-candidate-execution-records",
        "Candidate Execution Record Fields",
        FIELDS,
        List(List("[[ChosenValues]]", "a List of Chosen Value Records", "...")),
      ),
      // not a field table
      Table(
        "table-module-fields-example",
        "Module fields after module _A_ finishes executing",
        List("Field Module", "_A_", "[[Status]]"),
        List(List("_A_", "~evaluated~", "...")),
      ),
    ).map(t => t.id -> t).toMap

    // glossary definitions in the style of ECMA-262
    val dfns = List(
      Dfn("Completion Record", List("Completion Records")),
      Dfn("PrivateElements", List("PrivateElement")),
      Dfn("Async-from-Sync Iterator", List("Async-from-Sync Iterator object")),
      Dfn(
        "bound function exotic object",
        List("bound function exotic objects"),
      ),
      Dfn("AsyncGeneratorRequest", List("AsyncGeneratorRequests")),
      Dfn("WriteSharedMemory"),
      Dfn("candidate execution", List("candidate executions")),
    )

    checkEqual("decls")(
      // sorted by table ids
      TyModelExtractor(tables, dfns, Parser).decls -> List(
        TyDecl(
          "AsyncGeneratorRequest", // canonicalized modulo `Record`
          None,
          List(Field("Completion", false, "Completion")),
        ),
        TyDecl(
          "CyclicModuleRecord",
          None,
          List(Field("HasTLA", false, "Boolean")),
        ),
        TyDecl(
          "AsyncFromSyncIterator",
          None,
          List(Field("SyncIteratorRecord", false, "Record[IteratorRecord]")),
        ),
        TyDecl(
          "CandidateExecutionRecord", // lowercase definitions are not names
          None,
          List(Field("ChosenValues", false, "List[Record[ChosenValueRecord]]")),
        ),
        TyDecl(
          "CompletionRecord",
          None,
          List(
            Field("Type", false, "Enum[~break~, ~normal~, ~throw~]"),
            Field("Value", false, "Any"), // unknown type descriptions
            Field("Target", false, "Enum[~empty~] | String"),
          ),
        ),
        TyDecl(
          "BoundFunctionExoticObject",
          None,
          List(Field("BoundArguments", false, "List[ESValue]")),
        ),
        TyDecl(
          "PrivateElement", // the value column is not the second one
          None,
          List(
            Field("Key", false, "Record[PrivateName]"),
            Field("Get", false, "Record[FunctionObject] | Undefined"),
          ),
        ),
        TyDecl(
          "WriteSharedMemory", // canonicalized modulo `Event`
          None,
          List(Field("NoTear", false, "Boolean")),
        ),
      ),
    )
  }

  init
}
