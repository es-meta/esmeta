package esmeta.ty

import esmeta.util.BaseUtils.*
import scala.annotation.tailrec

/** type modeling */
// TODO consider refactoring
case class TyModel(infos: Map[String, TyInfo] = Map()) {

  /** get method map */
  // TODO optimize
  def getMethod(tname: String): Map[String, String] = infos.get(tname) match {
    case Some(info) =>
      val parentMethods = info.parent.map(getMethod).getOrElse(Map())
      parentMethods ++ info.methods
    case None => Map()
  }

  /** direct subtypes */
  private lazy val directSubTys: Map[String, Set[String]] = {
    var children = Map[String, Set[String]]()
    for {
      (name, info) <- infos
      parent <- info.parent
      set = children.getOrElse(parent, Set())
    } children += parent -> (set + name)
    children
  }

  /** subtypes */
  lazy val subTys: Map[String, Set[String]] = {
    var descs = Map[String, Set[String]]()
    def aux(name: String): Set[String] = descs.get(name) match {
      case Some(set) => set
      case None =>
        val set = (for {
          sub <- directSubTys.getOrElse(name, Set())
          elem <- aux(sub)
        } yield elem) + name
        descs += name -> set
        set
    }
    infos.collect { case (name, TyInfo(None, _, _)) => aux(name) }
    descs
  }
  def isSubTy(t0: String, t1: String): Boolean = subTys(t1) contains t0

  /** property map alias */
  type PropMap = Map[String, ValueTy]

  /** get types of property */
  def getProp(tname: String, p: String, check: Boolean): ValueTy =
    if (tname == "IntrinsicsRecord" && p.startsWith("%") && p.endsWith("%"))
      RecordT("Object")
    else
      propMap(tname).getOrElse(
        p, {
          if (check) warn(s"unknown property access: $tname.$p")
          AbsentT
        },
      )

  /** property type */
  private lazy val propMap: Map[String, PropMap] = (for {
    name <- infos.keySet
  } yield name -> getPropMap(name)).toMap

  /** get property map */
  private def getPropMap(name: String): PropMap =
    val upper = getUpperPropMap(name)
    val lower = getLowerPropMap(name)
    lower.foldLeft(upper) {
      case (map, (k, t)) =>
        val newT = t | map.getOrElse(k, BotT)
        map + (k -> newT)
    }

  /** get property map from ancestors */
  private def getUpperPropMap(name: String): PropMap = infos.get(name) match
    case Some(info) =>
      val parentProps = info.parent.map(getUpperPropMap).getOrElse(Map())
      val props = info.props
      weakMerge(parentProps, props)
    case None => Map()

  /** get property map of name */
  private def getSamePropMap(name: String): PropMap =
    infos.get(name).map(_.props).getOrElse(Map())

  /** get property map from ancestors */
  private def getLowerPropMap(name: String): PropMap =
    directSubTys.get(name) match
      case Some(children) =>
        children
          .map(child => {
            val lower = getLowerPropMap(child)
            val props = getSamePropMap(child)
            weakMerge(lower, props)
          })
          .reduce(parallelWeakMerge)
      case None => getSamePropMap(name)

  /** weak merge */
  private def weakMerge(lmap: PropMap, rmap: PropMap): PropMap = {
    val keys = lmap.keySet ++ rmap.keySet
    keys.toList
      .map(k => {
        val lt = lmap.getOrElse(k, BotT)
        val rt = rmap.getOrElse(k, BotT)
        k -> (lt | rt)
      })
      .toMap
  }

  /** parallel weak merge */
  private def parallelWeakMerge(lmap: PropMap, rmap: PropMap): PropMap = {
    val keys = lmap.keySet ++ rmap.keySet
    keys.toList
      .map(k => {
        val lt = lmap.getOrElse(k, AbsentT)
        val rt = rmap.getOrElse(k, AbsentT)
        k -> (lt | rt)
      })
      .toMap
  }

}
object TyModel {

  /** alias */
  val EMPTY = ConstT("empty")
  val UNRESOLVABLE = ConstT("unresolvable")
  val LEXICAL = ConstT("lexical")
  val INITIALIZED = ConstT("initialized")
  val UNINITIALIZED = ConstT("uninitialized")
  val FIELD = ConstT("field")
  val METHOD = ConstT("method")
  val ACCESSOR = ConstT("accessor")
  val BASE = ConstT("base")
  val DERIVED = ConstT("derived")
  val STRICT = ConstT("strict")
  val GLOBAL = ConstT("global")
  val UNLINKED = ConstT("unlinked")
  val LINKING = ConstT("linking")
  val LINKED = ConstT("linked")
  val EVALUATING = ConstT("evaluating")
  val EVALUATING_ASYNC = ConstT("evaluating-async")
  val EVALUATED = ConstT("evaluated")
  val NUMBER = ConstT("Number")
  val BIGINT = ConstT("BigInt")
  val ALL = ConstT("all")
  val ALL_BUT_DEFAULT = ConstT("all-but-default")
  val NORMAL = ConstT("normal")
  val BREAK = ConstT("break")
  val CONTINUE = ConstT("continue")
  val RETURN = ConstT("return")
  val THROW = ConstT("throw")
  val SUSPENDED_START = ConstT("suspendedStart")
  val SUSPENDED_YIELD = ConstT("suspendedYield")
  val EXECUTING = ConstT("executing")
  val AWAITING_RETURN = ConstT("awaitingDASHreturn")
  val COMPLETED = ConstT("completed")
  val PENDING = ConstT("pending")
  val FULFILLED = ConstT("fulfilled")
  val REJECTED = ConstT("rejected")
  val FULFILL = ConstT("Fulfill")
  val REJECT = ConstT("Reject")
  val NAMESPACE_OBJ = ConstT("namespace-object")
  val NAMESPACE = ConstT("NAMESPACE")

  // TODO extract type model from spec
  lazy val es: TyModel = TyModel(
    Map(
      // property descriptor
      "PropertyDescriptor" -> TyInfo(
        fields = Map(
          "Value" -> (ESValueT | AbsentT),
          "Writable" -> (BoolT | AbsentT),
          "Get" -> (RecordT("FunctionObject") | UndefT | AbsentT),
          "Set" -> (RecordT("FunctionObject") | UndefT | AbsentT),
          "Enumerable" -> (BoolT | AbsentT),
          "Configurable" -> (BoolT | AbsentT),
        ),
      ),

      // realm record
      "RealmRecord" -> TyInfo(
        fields = Map(
          "Intrinsics" -> RecordT("IntrinsicsRecord"),
          "GlobalObject" -> (UndefT | RecordT("Object")),
          "GlobalEnv" -> RecordT("GlobalEnvironmentRecord"),
          "TemplateMap" -> ListT(RecordT("TemplatePair")),
          "HostDefined" -> UndefT,
        ),
      ),
      "TemplatePair" -> TyInfo(
        fields = Map(
          "Site" -> AstT("TemplateLiteral"),
          "Array" -> RecordT("Object"),
        ),
      ),

      // execution contexts
      "ExecutionContext" -> TyInfo(
        fields = Map(
          "Function" -> (RecordT("FunctionObject") | NullT),
          "Realm" -> RecordT("RealmRecord"),
          "ScriptOrModule" -> (
            RecordT("ScriptRecord") |
            RecordT("ModuleRecord") |
            NullT
          ),
          "LexicalEnvironment" -> RecordT("EnvironmentRecord"),
          "VariableEnvironment" -> RecordT("EnvironmentRecord"),
          "PrivateEnvironment" -> (
            RecordT("PrivateEnvironmentRecord") |
            NullT
          ),
          "Generator" -> RecordT("Object"),
        ),
      ),

      // reference record
      "ReferenceRecord" -> TyInfo(
        fields = Map(
          "Base" -> (ESValueT | RecordT("EnvironmentRecord") | UNRESOLVABLE),
          "ReferencedName" -> (StrTopT | SymbolT | RecordT("PrivateName")),
          "Strict" -> BoolT,
          "ThisValue" -> (ESValueT | EMPTY),
        ),
      ),

      // private name
      "PrivateName" -> TyInfo(
        fields = Map("Description" -> StrTopT),
      ),

      // private element
      "PrivateElement" -> TyInfo(
        fields = Map(
          "Key" -> RecordT("PrivateName"),
          "Kind" -> (FIELD | METHOD | ACCESSOR),
          "Value" -> (AbsentT | ESValueT),
          "Get" -> (RecordT("FunctionObject") | UndefT | AbsentT),
          "Set" -> (RecordT("FunctionObject") | UndefT | AbsentT),
        ),
      ),

      // class field definition record
      "ClassFieldDefinitionRecord" -> TyInfo(
        fields = Map(
          "Name" -> (RecordT("PrivateName") | StrTopT | SymbolT),
          "Initializer" -> (RecordT("FunctionObject") | EMPTY),
        ),
      ),

      // class static block definition record
      "ClassStaticBlockDefinitionRecord" -> TyInfo(
        fields = Map("BodyFunction" -> RecordT("FunctionObject")),
      ),

      // iterator record
      "IteratorRecord" -> TyInfo(
        fields = Map(
          "Iterator" -> RecordT("OrdinaryObject"),
          "NextMethod" -> RecordT("FunctionObject"),
          "Done" -> BoolT,
        ),
      ),

      // objects
      "Object" -> TyInfo(
        parent = None,
        methods = Map(
          "GetPrototypeOf" -> "OrdinaryObject.GetPrototypeOf",
          "SetPrototypeOf" -> "OrdinaryObject.SetPrototypeOf",
          "IsExtensible" -> "OrdinaryObject.IsExtensible",
          "PreventExtensions" -> "OrdinaryObject.PreventExtensions",
          "GetOwnProperty" -> "OrdinaryObject.GetOwnProperty",
          "DefineOwnProperty" -> "OrdinaryObject.DefineOwnProperty",
          "HasProperty" -> "OrdinaryObject.HasProperty",
          "Get" -> "OrdinaryObject.Get",
          "Set" -> "OrdinaryObject.Set",
          "Delete" -> "OrdinaryObject.Delete",
          "OwnPropertyKeys" -> "OrdinaryObject.OwnPropertyKeys",
        ),
        fields = Map(
          "Extensible" -> BoolT,
          "Prototype" -> (RecordT("Object") | NullT),
          "SubMap" -> SubMapT(StrTopT | SymbolT, RecordT("PropertyDescriptor")),
        ),
      ),
      "OrdinaryObject" -> TyInfo(
        parent = Some("Object"),
        fields = Map(
          "ParameterMap" -> UndefT,
        ),
      ),
      "FunctionObject" -> TyInfo(
        parent = Some("OrdinaryObject"),
      ),
      "ECMAScriptFunctionObject" -> TyInfo(
        parent = Some("FunctionObject"),
        methods = Map(
          "Call" -> "ECMAScriptFunctionObject.Call",
          "Construct" -> "ECMAScriptFunctionObject.Construct",
        ),
        fields = Map(
          "Environment" -> RecordT("EnvironmentRecord"),
          "PrivateEnvironment" -> (
            RecordT("PrivateEnvironmentRecord") |
            NullT
          ),
          "FormalParameters" -> AstTopT,
          "ECMAScriptCode" -> AstTopT,
          "ConstructorKind" -> (BASE | DERIVED),
          "Realm" -> RecordT("RealmRecord"),
          "ScriptOrModule" -> (
            RecordT("ScriptRecord") |
            RecordT("ModuleRecord") |
            NullT
          ),
          "ThisMode" -> (LEXICAL | STRICT | GLOBAL),
          "Strict" -> BoolT,
          "HomeObject" -> (RecordT("Object") | UndefT),
          "SourceText" -> StrTopT,
          "Fields" -> ListT(RecordT("ClassFieldDefinitionRecord")),
          "PrivateMethods" -> ListT(RecordT("PrivateElement")),
          "ClassFieldInitializerName" ->
          (StrTopT | SymbolT | RecordT("PrivateName") | EMPTY),
          "IsClassConstructor" -> BoolT,
        ),
      ),
      "BuiltinFunctionObject" -> TyInfo(
        parent = Some("FunctionObject"),
        methods = Map(
          "Call" -> "BuiltinFunctionObject.Call",
          // XXX "Construct" -> "BuiltinFunctionObject.Construct",
        ),
        fields = Map(
          "Code" -> CloTopT,
          "Realm" -> RecordT("RealmRecord"),
          "InitialName" -> (NullT | StrTopT),
        ),
      ),
      "BoundFunctionExoticObject" -> TyInfo(
        parent = Some("Object"),
        methods = Map(
          "Call" -> "BoundFunctionExoticObject.Call",
          "Construct" -> "BoundFunctionExoticObject.Construct",
        ),
        fields = Map(
          "BoundTargetFunction" -> RecordT("FunctionObject"),
          "BoundThis" -> ESValueT,
          "BoundArguments" -> ListT(ESValueT),
        ),
      ),
      "ArrayExoticObject" -> TyInfo(
        parent = Some("Object"),
        methods = Map(
          "DefineOwnProperty" -> "ArrayExoticObject.DefineOwnProperty",
        ),
      ),
      "StringExoticObject" -> TyInfo(
        parent = Some("Object"),
        methods = Map(
          "GetOwnProperty" -> "StringExoticObject.GetOwnProperty",
          "DefineOwnProperty" ->
          "StringExoticObject.DefineOwnProperty",
          "OwnPropertyKeys" -> "StringExoticObject.OwnPropertyKeys",
        ),
        fields = Map("StringData" -> StrTopT),
      ),
      "ArgumentsExoticObject" -> TyInfo(
        parent = Some("Object"),
        methods = Map(
          "GetOwnProperty" -> "ArgumentsExoticObject.GetOwnProperty",
          "DefineOwnProperty" ->
          "ArgumentsExoticObject.DefineOwnProperty",
          "Get" -> "ArgumentsExoticObject.Get",
          "Set" -> "ArgumentsExoticObject.Set",
          "Delete" -> "ArgumentsExoticObject.Delete",
        ),
        fields = Map(
          "ParameterMap" -> (RecordT("OrdinaryObject") | NullT),
        ),
      ),
      "IntegerIndexedExoticObject" -> TyInfo(
        parent = Some("Object"),
        methods = Map(
          "GetOwnProperty" ->
          "IntegerIndexedExoticObject.GetOwnProperty",
          "HasProperty" -> "IntegerIndexedExoticObject.HasProperty",
          "DefineOwnProperty" ->
          "IntegerIndexedExoticObject.DefineOwnProperty",
          "Get" -> "IntegerIndexedExoticObject.Get",
          "Set" -> "IntegerIndexedExoticObject.Set",
          "Delete" -> "IntegerIndexedExoticObject.Delete",
          "OwnPropertyKeys" ->
          "IntegerIndexedExoticObject.OwnPropertyKeys",
        ),
        fields = Map(
          "ViewedArrayBuffer" -> RecordT("ArrayBufferObject"),
          "ArrayLength" -> MathT,
          "ByteOffset" -> MathT,
          "ContentTy" -> (NUMBER | BIGINT),
          "TydArrayName" -> StrTopT,
        ),
      ),
      "ModuleNamespaceExoticObject" -> TyInfo(
        parent = Some("Object"),
        methods = Map(
          "GetPrototypeOf" -> "ModuleNamespaceExoticObject.GetPrototypeOf",
          "SetPrototypeOf" -> "ModuleNamespaceExoticObject.SetPrototypeOf",
          "IsExtensible" -> "ModuleNamespaceExoticObject.IsExtensible",
          "PreventExtensions" -> "ModuleNamespaceExoticObject.PreventExtensions",
          "GetOwnProperty" -> "ModuleNamespaceExoticObject.GetOwnProperty",
          "DefineOwnProperty" -> "ModuleNamespaceExoticObject.DefineOwnProperty",
          "HasProperty" -> "ModuleNamespaceExoticObject.HasProperty",
          "Get" -> "ModuleNamespaceExoticObject.Get",
          "Set" -> "ModuleNamespaceExoticObject.Set",
          "Delete" -> "ModuleNamespaceExoticObject.Delete",
          "OwnPropertyKeys" -> "ModuleNamespaceExoticObject.OwnPropertyKeys",
        ),
        fields = Map(
          "Module" -> RecordT("ModuleRecord"),
          "Exports" -> ListT(StrTopT),
        ),
      ),
      "ImmutablePrototypeExoticObject" -> TyInfo(
        parent = Some("Object"),
        methods = Map(
          "SetPrototypeOf" ->
          "ImmutablePrototypeExoticObject.SetPrototypeOf",
        ),
      ),
      "ProxyExoticObject" -> TyInfo(
        parent = Some("Object"),
        methods = Map(
          "GetPrototypeOf" -> "ProxyExoticObject.GetPrototypeOf",
          "SetPrototypeOf" -> "ProxyExoticObject.SetPrototypeOf",
          "IsExtensible" -> "ProxyExoticObject.IsExtensible",
          "PreventExtensions" -> "ProxyExoticObject.PreventExtensions",
          "GetOwnProperty" -> "ProxyExoticObject.GetOwnProperty",
          "DefineOwnProperty" -> "ProxyExoticObject.DefineOwnProperty",
          "HasProperty" -> "ProxyExoticObject.HasProperty",
          "Get" -> "ProxyExoticObject.Get",
          "Set" -> "ProxyExoticObject.Set",
          "Delete" -> "ProxyExoticObject.Delete",
          "OwnPropertyKeys" -> "ProxyExoticObject.OwnPropertyKeys",
          "Call" -> "ProxyExoticObject.Call",
          "Construct" -> "ProxyExoticObject.Construct",
        ),
        fields = Map(
          "ProxyHandler" -> (RecordT("Object") | NullT),
          "ProxyTarget" -> (RecordT("Object") | NullT),
        ),
      ),
      "ArrayBufferObject" -> TyInfo(parent = Some("Object")),
      "BooleanObject" -> TyInfo(parent = Some("OrdinaryObject")),
      "BigIntObject" -> TyInfo(parent = Some("OrdinaryObject")),
      "NumberObject" -> TyInfo(parent = Some("OrdinaryObject")),
      "SymbolObject" -> TyInfo(parent = Some("OrdinaryObject")),

      // special instances
      "ForInIteratorInstance" -> TyInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "Object" -> RecordT("Object"),
          "ObjectWasVisited" -> BoolT,
          "VisitedKeys" -> ListT(StrTopT),
          "RemainingKeys" -> ListT(StrTopT),
        ),
      ),
      "AsyncFromSyncIteratorInstance" -> TyInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "SyncIteratorRecord" -> RecordT("IteratorRecord"),
        ),
      ),
      "PromiseCapabilityRecord" -> TyInfo(
        fields = Map(
          "Promise" -> RecordT("Object"),
          "Resovle" -> RecordT("FunctionObject"),
          "Reject" -> RecordT("FunctionObject"),
        ),
      ),
      "PromiseReaction" -> TyInfo(
        fields = Map(
          "Capability" -> (RecordT("PromiseCapabilityRecord") | UndefT),
          "Ty" -> (FULFILL | REJECT),
          "Handler" -> (RecordT("JobCallbackRecord") | EMPTY),
        ),
      ),
      "PromiseInstance" -> TyInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "PromiseState" -> (PENDING | FULFILLED | REJECTED),
          "PromiseResult" -> ESValueT,
          "PromiseFulfillReactions" -> ListT(RecordT("PromiseReaction")),
          "PromiseRejectReactions" -> ListT(RecordT("PromiseReaction")),
          "PromiseIsHandled" -> BoolT,
        ),
      ),
      "GeneratorInstance" -> TyInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "GeneratorState" -> (
            UndefT |
            SUSPENDED_START |
            SUSPENDED_YIELD |
            EXECUTING |
            COMPLETED
          ),
          "GeneratorContext" -> RecordT("ExecutionContext"),
          "GeneratorBrand" -> (StrTopT | EMPTY),
        ),
      ),
      "AsyncGeneratorInstance" -> TyInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "AsyncGeneratorState" -> (
            UndefT |
            SUSPENDED_START |
            SUSPENDED_YIELD |
            EXECUTING |
            AWAITING_RETURN |
            COMPLETED
          ),
          "AsyncGeneratorContext" -> RecordT("ExecutionContext"),
          "AsyncGeneratorQueue" -> ListT(
            RecordT("AsyncGeneratorRequestRecord"),
          ),
          "GeneratorBrand" -> (StrTopT | EMPTY),
        ),
      ),
      "AsyncGeneratorRequestRecord" -> TyInfo(
        fields = Map(
          // TODO "Completion" -> (NormalT(_) | AbruptT),
          "Capability" -> RecordT("PromiseCapabilityRecord"),
        ),
      ),

      // environment records
      "EnvironmentRecord" -> TyInfo(
        fields = Map(
          "OuterEnv" -> (NullT | RecordT("EnvironmentRecord")),
          "SubMap" -> SubMapT(StrTopT, RecordT("Binding")),
        ),
      ),
      "Binding" -> TyInfo(
        fields = Map(
          "BoundValue" -> ESValueT,
          "initialized" -> BoolT,
        ),
      ),
      "MutableBinding" -> TyInfo(parent = Some("Binding")),
      "ImmutableBinding" -> TyInfo(
        parent = Some("Binding"),
        fields = Map("strict" -> BoolT),
      ),
      "DeclarativeEnvironmentRecord" -> TyInfo(
        parent = Some("EnvironmentRecord"),
        methods = Map(
          "HasBinding" -> "DeclarativeEnvironmentRecord.HasBinding",
          "CreateMutableBinding" ->
          "DeclarativeEnvironmentRecord.CreateMutableBinding",
          "CreateImmutableBinding" ->
          "DeclarativeEnvironmentRecord.CreateImmutableBinding",
          "InitializeBinding" ->
          "DeclarativeEnvironmentRecord.InitializeBinding",
          "SetMutableBinding" ->
          "DeclarativeEnvironmentRecord.SetMutableBinding",
          "GetBindingValue" ->
          "DeclarativeEnvironmentRecord.GetBindingValue",
          "DeleteBinding" ->
          "DeclarativeEnvironmentRecord.DeleteBinding",
          "HasThisBinding" ->
          "DeclarativeEnvironmentRecord.HasThisBinding",
          "HasSuperBinding" ->
          "DeclarativeEnvironmentRecord.HasSuperBinding",
          "WithBaseObject" ->
          "DeclarativeEnvironmentRecord.WithBaseObject",
        ),
      ),
      "FunctionEnvironmentRecord" -> TyInfo(
        parent = Some("DeclarativeEnvironmentRecord"),
        methods = Map(
          "BindThisValue" -> "FunctionEnvironmentRecord.BindThisValue",
          "HasThisBinding" ->
          "FunctionEnvironmentRecord.HasThisBinding",
          "HasSuperBinding" ->
          "FunctionEnvironmentRecord.HasSuperBinding",
          "GetThisBinding" ->
          "FunctionEnvironmentRecord.GetThisBinding",
          "GetSuperBase" -> "FunctionEnvironmentRecord.GetSuperBase",
        ),
        fields = Map(
          "ThisValue" -> ESValueT,
          "ThisBindingStatus" -> (LEXICAL | INITIALIZED | UNINITIALIZED),
          "FunctionObject" -> RecordT("FunctionObject"),
          "NewTarget" -> (RecordT("Object") | UndefT),
        ),
      ),
      "ModuleEnvironmentRecord" -> TyInfo(
        parent = Some("DeclarativeEnvironmentRecord"),
        methods = Map(
          "GetBindingValue" -> "ModuleEnvironmentRecord.GetBindingValue",
          "HasThisBinding" ->
          "ModuleEnvironmentRecord.HasThisBinding",
          "GetThisBinding" ->
          "ModuleEnvironmentRecord.GetThisBinding",
          "CreateImportBinding" -> "ModuleEnvironmentRecord.CreateImportBinding",
        ),
      ),
      "ObjectEnvironmentRecord" -> TyInfo(
        parent = Some("EnvironmentRecord"),
        methods = Map(
          "HasBinding" -> "ObjectEnvironmentRecord.HasBinding",
          "CreateMutableBinding" ->
          "ObjectEnvironmentRecord.CreateMutableBinding",
          "InitializeBinding" ->
          "ObjectEnvironmentRecord.InitializeBinding",
          "SetMutableBinding" ->
          "ObjectEnvironmentRecord.SetMutableBinding",
          "GetBindingValue" ->
          "ObjectEnvironmentRecord.GetBindingValue",
          "DeleteBinding" -> "ObjectEnvironmentRecord.DeleteBinding",
          "HasThisBinding" -> "ObjectEnvironmentRecord.HasThisBinding",
          "HasSuperBinding" ->
          "ObjectEnvironmentRecord.HasSuperBinding",
          "WithBaseObject" -> "ObjectEnvironmentRecord.WithBaseObject",
        ),
        fields = Map(
          "IsWithEnvironment" -> BoolT,
          "BindingObject" -> RecordT("Object"),
        ),
      ),
      "GlobalEnvironmentRecord" -> TyInfo(
        parent = Some("EnvironmentRecord"),
        methods = Map(
          "HasBinding" -> "GlobalEnvironmentRecord.HasBinding",
          "CreateMutableBinding" ->
          "GlobalEnvironmentRecord.CreateMutableBinding",
          "CreateImmutableBinding" ->
          "GlobalEnvironmentRecord.CreateImmutableBinding",
          "InitializeBinding" ->
          "GlobalEnvironmentRecord.InitializeBinding",
          "SetMutableBinding" ->
          "GlobalEnvironmentRecord.SetMutableBinding",
          "GetBindingValue" ->
          "GlobalEnvironmentRecord.GetBindingValue",
          "DeleteBinding" -> "GlobalEnvironmentRecord.DeleteBinding",
          "HasThisBinding" -> "GlobalEnvironmentRecord.HasThisBinding",
          "HasSuperBinding" ->
          "GlobalEnvironmentRecord.HasSuperBinding",
          "WithBaseObject" -> "GlobalEnvironmentRecord.WithBaseObject",
          "GetThisBinding" -> "GlobalEnvironmentRecord.GetThisBinding",
          "HasVarDeclaration" ->
          "GlobalEnvironmentRecord.HasVarDeclaration",
          "HasLexicalDeclaration" ->
          "GlobalEnvironmentRecord.HasLexicalDeclaration",
          "HasRestrictedGlobalProperty" ->
          "GlobalEnvironmentRecord.HasRestrictedGlobalProperty",
          "CanDeclareGlobalVar" ->
          "GlobalEnvironmentRecord.CanDeclareGlobalVar",
          "CanDeclareGlobalFunction" ->
          "GlobalEnvironmentRecord.CanDeclareGlobalFunction",
          "CreateGlobalVarBinding" ->
          "GlobalEnvironmentRecord.CreateGlobalVarBinding",
          "CreateGlobalFunctionBinding" ->
          "GlobalEnvironmentRecord.CreateGlobalFunctionBinding",
        ),
        fields = Map(
          // XXX "OuterEnv" -> NullT,
          "ObjectRecord" -> RecordT("ObjectEnvironmentRecord"),
          "GlobalThisValue" -> RecordT("Object"),
          "DeclarativeRecord" -> RecordT("DeclarativeEnvironmentRecord"),
          "VarNames" -> ListT(StrTopT),
        ),
      ),

      // private environment record
      "PrivateEnvironmentRecord" -> TyInfo(
        fields = Map(
          "OuterPrivateEnvironment" -> (
            RecordT("PrivateEnvironmentRecord") |
            NullT
          ),
          "Names" -> ListT(RecordT("PrivateName")),
        ),
      ),

      // job callback record
      "JobCallbackRecord" -> TyInfo(
        fields = Map(
          "Callback" -> RecordT("FunctionObject"),
          "HostDefined" -> UndefT,
        ),
      ),

      // agent record
      "AgentRecord" -> TyInfo(
        fields = Map(
          "LittleEndian" -> BoolT,
          "CanBlock" -> BoolT,
          "Signifier" -> RecordT("AgentSignifier"),
          "IsLockFree1" -> BoolT,
          "IsLockFree2" -> BoolT,
          "IsLockFree8" -> BoolT,
          "CandidateExecution" -> RecordT("CandidateExecutionRecord"),
          "KeptAlive" -> ListT(RecordT("Object")),
        ),
      ),

      // script record
      "ScriptRecord" -> TyInfo(
        fields = Map(
          "Realm" -> (RecordT("RealmRecord") | UndefT),
          "ECMAScriptCode" -> AstT("Script"),
          "HostDefined" -> EMPTY,
        ),
      ),

      // module record
      "ModuleRecord" -> TyInfo(
        fields = Map(
          "Realm" -> RecordT("RealmRecord"),
          "Environment" -> (RecordT("ModuleEnvironmentRecord") | EMPTY),
          "Namespace" -> (RecordT("ModuleNamespaceExoticObject") | EMPTY),
          "HostDefined" -> UndefT,
        ),
      ),
      "CyclicModuleRecord" -> TyInfo(
        parent = Some("ModuleRecord"),
        methods = Map(
          "Link" -> "CyclicModuleRecord.Link",
          "Evaluate" -> "CyclicModuleRecord.Evaluate",
        ),
        fields = Map(
          "Status" ->
          (
            UNLINKED |
            LINKING |
            LINKED |
            EVALUATING |
            EVALUATING_ASYNC |
            EVALUATED
          ),
          "EvaluationError" -> (AbruptT | EMPTY),
          "DFSIndex" -> (MathT | EMPTY),
          "DFSAncestorIndex" -> (MathT | EMPTY),
          "RequestedModules" -> ListT(StrTopT),
          "CycleRoot" -> (RecordT("CyclicModuleRecord") | EMPTY),
          "HasTLA" -> BoolT,
          "AsyncEvaluation" -> BoolT,
          "TopLevelCapability" -> (
            RecordT("PromiseCapabilityRecord") |
            EMPTY,
          ),
          "AsyncParentModules" -> ListT(RecordT("CyclicModuleRecord")),
          "PendingAsyncDependencies" -> (MathT | EMPTY),
        ),
      ),
      "SourceTextModuleRecord" -> TyInfo(
        parent = Some("CyclicModuleRecord"),
        methods = Map(
          "GetExportedNames" -> "SourceTextModuleRecord.GetExportedNames",
          "ResolveExport" -> "SourceTextModuleRecord.ResolveExport",
          "InitializeEnvironment" -> "SourceTextModuleRecord.InitializeEnvironment",
          "ExecuteModule" -> "SourceTextModuleRecord.ExecuteModule",
        ),
        fields = Map(
          "ECMAScriptCode" -> AstT("Module"),
          "Context" -> RecordT("ExecutionContext"),
          "ImportMeta" -> (RecordT("Object") | EMPTY),
          "ImportEntries" -> ListT(RecordT("ImportEntryRecord")),
          "LocalExportEntries" -> ListT(RecordT("ExportEntryRecord")),
          "IndirectExportEntries" -> ListT(RecordT("ExportEntryRecord")),
          "StarExportEntries" -> ListT(RecordT("ExportEntryRecord")),
        ),
      ),
      "ImportEntryRecord" -> TyInfo(
        fields = Map(
          "ModuleRequest" -> StrTopT,
          "ImportName" -> (StrTopT | NAMESPACE_OBJ),
          "LocalName" -> StrTopT,
        ),
      ),
      "ExportEntryRecord" -> TyInfo(
        fields = Map(
          "ExportName" -> (StrTopT | NullT),
          "ModuleRequest" -> (StrTopT | NullT),
          "ImportName" -> (StrTopT | NullT | ALL | ALL_BUT_DEFAULT),
          "LocalName" -> (StrTopT | NullT),
        ),
      ),
      "ResolvedBindingRecord" -> TyInfo(
        fields = Map(
          "Module" -> RecordT("ModuleRecord"),
          "BindingName" -> (StrTopT | NAMESPACE),
        ),
      ),

      // symbol registry
      "GlobalSymbolRegistryRecord" -> TyInfo(
        fields = Map(
          "Key" -> StrTopT,
          "Symbol" -> SymbolT,
        ),
      ),

      // match record
      "MatchRecord" -> TyInfo(
        fields = Map(
          "StartIndex" -> MathT,
          "EndIndex" -> MathT,
        ),
      ),

      // pending job
      "PendingJob" -> TyInfo(
        fields = Map(
          "Job" -> CloTopT,
          "Realm" -> RecordT("RealmRecord"),
          "ScriptOrModule" ->
          (RecordT("ScriptRecord") | RecordT("ModuleRecord") | NullT),
        ),
      ),
    ),
  )
}

/** type information */
case class TyInfo(
  parent: Option[String] = None,
  methods: Map[String, String] = Map(),
  fields: Map[String, ValueTy] = Map(),
) {
  lazy val props: Map[String, ValueTy] =
    val keys = methods.keySet ++ fields.keySet
    (for {
      k <- keys
      fs = fields.getOrElse(k, BotT)
      tys = methods.get(k) match
        case None         => fs
        case Some(method) => fs | CloT(method)
    } yield k -> tys).toMap
}
