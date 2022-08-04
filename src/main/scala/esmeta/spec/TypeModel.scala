package esmeta.spec

import scala.annotation.tailrec
import esmeta.analyzer.domain.*
import esmeta.analyzer.util.warning

/** type modeling */
// TODO consider refactoring
case class TypeModel(infos: Map[String, TypeInfo] = Map()) {

  /** get method map */
  // TODO optimize
  def getMethod(tname: String): Map[String, String] = infos.get(tname) match {
    case Some(info) =>
      val parentMethods = info.parent.map(getMethod).getOrElse(Map())
      parentMethods ++ info.methods
    case None => Map()
  }

  /** direct subtypes */
  private lazy val directSubTypes: Map[String, Set[String]] = {
    var children = Map[String, Set[String]]()
    for {
      (name, info) <- infos
      parent <- info.parent
      set = children.getOrElse(parent, Set())
    } children += parent -> (set + name)
    children
  }

  /** subtypes */
  lazy val subTypes: Map[String, Set[String]] = {
    var descs = Map[String, Set[String]]()
    def aux(name: String): Set[String] = descs.get(name) match {
      case Some(set) => set
      case None =>
        val set = (for {
          sub <- directSubTypes.getOrElse(name, Set())
          elem <- aux(sub)
        } yield elem) + name
        descs += name -> set
        set
    }
    infos.collect { case (name, TypeInfo(None, _, _)) => aux(name) }
    descs
  }
  def isSubType(t0: String, t1: String): Boolean = subTypes(t1) contains t0

  /** property map alias */
  type PropMap = Map[String, Set[Type]]

  /** get types of property */
  def getProp(tname: String, p: String, check: Boolean): Set[Type] =
    if (tname == "IntrinsicsRecord" && p.startsWith("%") && p.endsWith("%"))
      Set(NameT("Object"))
    else
      propMap(tname).getOrElse(
        p, {
          if (check) warning(s"unknown property access: $tname.$p")
          Set(AbsentT)
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
        val newT = t ++ map.getOrElse(k, Set())
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
    directSubTypes.get(name) match
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
        val lt = lmap.getOrElse(k, Set())
        val rt = rmap.getOrElse(k, Set())
        k -> (lt ++ rt)
      })
      .toMap
  }

  /** parallel weak merge */
  private def parallelWeakMerge(lmap: PropMap, rmap: PropMap): PropMap = {
    val keys = lmap.keySet ++ rmap.keySet
    keys.toList
      .map(k => {
        val lt = lmap.getOrElse(k, Set(AbsentT))
        val rt = rmap.getOrElse(k, Set(AbsentT))
        k -> (lt ++ rt)
      })
      .toMap
  }

}
object TypeModel {

  /** conversion for type */
  given Conversion[Type, Set[Type]] = Set(_)

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
  lazy val js: TypeModel = TypeModel(
    Map(
      // property descriptor
      "PropertyDescriptor" -> TypeInfo(
        fields = Map(
          "Value" -> Set(ESValueT, AbsentT),
          "Writable" -> Set(BoolT, AbsentT),
          "Get" -> Set(NameT("FunctionObject"), UndefT, AbsentT),
          "Set" -> Set(NameT("FunctionObject"), UndefT, AbsentT),
          "Enumerable" -> Set(BoolT, AbsentT),
          "Configurable" -> Set(BoolT, AbsentT),
        ),
      ),

      // realm record
      "RealmRecord" -> TypeInfo(
        fields = Map(
          "Intrinsics" -> NameT("IntrinsicsRecord"),
          "GlobalObject" -> Set(UndefT, NameT("Object")),
          "GlobalEnv" -> NameT("GlobalEnvironmentRecord"),
          "TemplateMap" -> ListT(NameT("TemplatePair")),
          "HostDefined" -> UndefT,
        ),
      ),
      "TemplatePair" -> TypeInfo(
        fields = Map(
          "Site" -> AstT("TemplateLiteral"),
          "Array" -> NameT("Object"),
        ),
      ),

      // execution contexts
      "ExecutionContext" -> TypeInfo(
        fields = Map(
          "Function" -> Set(NameT("FunctionObject"), NullT),
          "Realm" -> NameT("RealmRecord"),
          "ScriptOrModule" ->
          Set(NameT("ScriptRecord"), NameT("ModuleRecord"), NullT),
          "LexicalEnvironment" -> NameT("EnvironmentRecord"),
          "VariableEnvironment" -> NameT("EnvironmentRecord"),
          "PrivateEnvironment" -> Set(NameT("PrivateEnvironmentRecord"), NullT),
          "Generator" -> NameT("Object"),
        ),
      ),

      // reference record
      "ReferenceRecord" -> TypeInfo(
        fields = Map(
          "Base" -> Set(ESValueT, NameT("EnvironmentRecord"), UNRESOLVABLE),
          "ReferencedName" -> Set(StrT, SymbolT, NameT("PrivateName")),
          "Strict" -> BoolT,
          "ThisValue" -> Set(ESValueT, EMPTY),
        ),
      ),

      // private name
      "PrivateName" -> TypeInfo(
        fields = Map("Description" -> StrT),
      ),

      // private element
      "PrivateElement" -> TypeInfo(
        fields = Map(
          "Key" -> NameT("PrivateName"),
          "Kind" -> Set(FIELD, METHOD, ACCESSOR),
          "Value" -> Set(AbsentT, ESValueT),
          "Get" -> Set(NameT("FunctionObject"), UndefT, AbsentT),
          "Set" -> Set(NameT("FunctionObject"), UndefT, AbsentT),
        ),
      ),

      // class field definition record
      "ClassFieldDefinitionRecord" -> TypeInfo(
        fields = Map(
          "Name" -> Set(NameT("PrivateName"), StrT, SymbolT),
          "Initializer" -> Set(NameT("FunctionObject"), EMPTY),
        ),
      ),

      // class static block definition record
      "ClassStaticBlockDefinitionRecord" -> TypeInfo(
        fields = Map("BodyFunction" -> NameT("FunctionObject")),
      ),

      // iterator record
      "IteratorRecord" -> TypeInfo(
        fields = Map(
          "Iterator" -> NameT("OrdinaryObject"),
          "NextMethod" -> NameT("FunctionObject"),
          "Done" -> BoolT,
        ),
      ),

      // objects
      "Object" -> TypeInfo(
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
          "Prototype" -> Set(NameT("Object"), NullT),
          "SubMap" -> MapT(NameT("PropertyDescriptor")),
        ),
      ),
      "OrdinaryObject" -> TypeInfo(
        parent = Some("Object"),
        fields = Map(
          "ParameterMap" -> UndefT,
        ),
      ),
      "FunctionObject" -> TypeInfo(
        parent = Some("OrdinaryObject"),
      ),
      "ECMAScriptFunctionObject" -> TypeInfo(
        parent = Some("FunctionObject"),
        methods = Map(
          "Call" -> "ECMAScriptFunctionObject.Call",
          "Construct" -> "ECMAScriptFunctionObject.Construct",
        ),
        fields = Map(
          "Environment" -> NameT("EnvironmentRecord"),
          "PrivateEnvironment" -> Set(NameT("PrivateEnvironmentRecord"), NullT),
          "FormalParameters" -> AstTopT,
          "ECMAScriptCode" -> AstTopT,
          "ConstructorKind" -> Set(BASE, DERIVED),
          "Realm" -> NameT("RealmRecord"),
          "ScriptOrModule" -> Set(
            NameT("ScriptRecord"),
            NameT("ModuleRecord"),
            NullT,
          ),
          "ThisMode" -> Set(LEXICAL, STRICT, GLOBAL),
          "Strict" -> BoolT,
          "HomeObject" -> Set(NameT("Object"), UndefT),
          "SourceText" -> StrT,
          "Fields" -> ListT(NameT("ClassFieldDefinitionRecord")),
          "PrivateMethods" -> ListT(NameT("PrivateElement")),
          "ClassFieldInitializerName" ->
          Set(StrT, SymbolT, NameT("PrivateName"), EMPTY),
          "IsClassConstructor" -> BoolT,
        ),
      ),
      "BuiltinFunctionObject" -> TypeInfo(
        parent = Some("FunctionObject"),
        methods = Map(
          "Call" -> "BuiltinFunctionObject.Call",
          // XXX "Construct" -> "BuiltinFunctionObject.Construct",
        ),
        fields = Map(
          "Code" -> CloTopT,
          "Realm" -> NameT("RealmRecord"),
          "InitialName" -> Set(NullT, StrT),
        ),
      ),
      "BoundFunctionExoticObject" -> TypeInfo(
        parent = Some("Object"),
        methods = Map(
          "Call" -> "BoundFunctionExoticObject.Call",
          "Construct" -> "BoundFunctionExoticObject.Construct",
        ),
        fields = Map(
          "BoundTargetFunction" -> NameT("FunctionObject"),
          "BoundThis" -> ESValueT,
          "BoundArguments" -> ListT(ESValueT),
        ),
      ),
      "ArrayExoticObject" -> TypeInfo(
        parent = Some("Object"),
        methods = Map(
          "DefineOwnProperty" -> "ArrayExoticObject.DefineOwnProperty",
        ),
      ),
      "StringExoticObject" -> TypeInfo(
        parent = Some("Object"),
        methods = Map(
          "GetOwnProperty" -> "StringExoticObject.GetOwnProperty",
          "DefineOwnProperty" ->
          "StringExoticObject.DefineOwnProperty",
          "OwnPropertyKeys" -> "StringExoticObject.OwnPropertyKeys",
        ),
        fields = Map("StringData" -> StrT),
      ),
      "ArgumentsExoticObject" -> TypeInfo(
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
          "ParameterMap" -> Set(NameT("OrdinaryObject"), NullT),
        ),
      ),
      "IntegerIndexedExoticObject" -> TypeInfo(
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
          "ViewedArrayBuffer" -> NameT("ArrayBufferObject"),
          "ArrayLength" -> MathT,
          "ByteOffset" -> MathT,
          "ContentType" -> Set(NUMBER, BIGINT),
          "TypedArrayName" -> StrT,
        ),
      ),
      "ModuleNamespaceExoticObject" -> TypeInfo(
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
          "Module" -> NameT("ModuleRecord"),
          "Exports" -> ListT(StrT),
        ),
      ),
      "ImmutablePrototypeExoticObject" -> TypeInfo(
        parent = Some("Object"),
        methods = Map(
          "SetPrototypeOf" ->
          "ImmutablePrototypeExoticObject.SetPrototypeOf",
        ),
      ),
      "ProxyExoticObject" -> TypeInfo(
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
          "ProxyHandler" -> Set(NameT("Object"), NullT),
          "ProxyTarget" -> Set(NameT("Object"), NullT),
        ),
      ),
      "ArrayBufferObject" -> TypeInfo(parent = Some("Object")),
      "BooleanObject" -> TypeInfo(parent = Some("OrdinaryObject")),
      "BigIntObject" -> TypeInfo(parent = Some("OrdinaryObject")),
      "NumberObject" -> TypeInfo(parent = Some("OrdinaryObject")),
      "SymbolObject" -> TypeInfo(parent = Some("OrdinaryObject")),

      // special instances
      "ForInIteratorInstance" -> TypeInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "Object" -> NameT("Object"),
          "ObjectWasVisited" -> BoolT,
          "VisitedKeys" -> ListT(StrT),
          "RemainingKeys" -> ListT(StrT),
        ),
      ),
      "AsyncFromSyncIteratorInstance" -> TypeInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "SyncIteratorRecord" -> NameT("IteratorRecord"),
        ),
      ),
      "PromiseCapabilityRecord" -> TypeInfo(
        fields = Map(
          "Promise" -> NameT("Object"),
          "Resovle" -> NameT("FunctionObject"),
          "Reject" -> NameT("FunctionObject"),
        ),
      ),
      "PromiseReaction" -> TypeInfo(
        fields = Map(
          "Capability" -> Set(NameT("PromiseCapabilityRecord"), UndefT),
          "Type" -> Set(FULFILL, REJECT),
          "Handler" -> Set(NameT("JobCallbackRecord"), EMPTY),
        ),
      ),
      "PromiseInstance" -> TypeInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "PromiseState" -> Set(PENDING, FULFILLED, REJECTED),
          "PromiseResult" -> ESValueT,
          "PromiseFulfillReactions" -> ListT(NameT("PromiseReaction")),
          "PromiseRejectReactions" -> ListT(NameT("PromiseReaction")),
          "PromiseIsHandled" -> BoolT,
        ),
      ),
      "GeneratorInstance" -> TypeInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "GeneratorState" -> Set(
            UndefT,
            SUSPENDED_START,
            SUSPENDED_YIELD,
            EXECUTING,
            COMPLETED,
          ),
          "GeneratorContext" -> NameT("ExecutionContext"),
          "GeneratorBrand" -> Set(StrT, EMPTY),
        ),
      ),
      "AsyncGeneratorInstance" -> TypeInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "AsyncGeneratorState" -> Set(
            UndefT,
            SUSPENDED_START,
            SUSPENDED_YIELD,
            EXECUTING,
            AWAITING_RETURN,
            COMPLETED,
          ),
          "AsyncGeneratorContext" -> NameT("ExecutionContext"),
          "AsyncGeneratorQueue" -> ListT(NameT("AsyncGeneratorRequestRecord")),
          "GeneratorBrand" -> Set(StrT, EMPTY),
        ),
      ),
      "AsyncGeneratorRequestRecord" -> TypeInfo(
        fields = Map(
          // TODO "Completion" -> Set(NormalT(_), AbruptT),
          "Capability" -> NameT("PromiseCapabilityRecord"),
        ),
      ),

      // environment records
      "EnvironmentRecord" -> TypeInfo(
        fields = Map(
          "OuterEnv" -> Set(NullT, NameT("EnvironmentRecord")),
          "SubMap" -> MapT(NameT("Binding")),
        ),
      ),
      "Binding" -> TypeInfo(
        fields = Map(
          "BoundValue" -> ESValueT,
          "initialized" -> BoolT,
        ),
      ),
      "MutableBinding" -> TypeInfo(parent = Some("Binding")),
      "ImmutableBinding" -> TypeInfo(
        parent = Some("Binding"),
        fields = Map("strict" -> BoolT),
      ),
      "DeclarativeEnvironmentRecord" -> TypeInfo(
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
      "FunctionEnvironmentRecord" -> TypeInfo(
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
          "ThisBindingStatus" -> Set(LEXICAL, INITIALIZED, UNINITIALIZED),
          "FunctionObject" -> NameT("FunctionObject"),
          "NewTarget" -> Set(NameT("Object"), UndefT),
        ),
      ),
      "ModuleEnvironmentRecord" -> TypeInfo(
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
      "ObjectEnvironmentRecord" -> TypeInfo(
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
          "BindingObject" -> NameT("Object"),
        ),
      ),
      "GlobalEnvironmentRecord" -> TypeInfo(
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
          "ObjectRecord" -> NameT("ObjectEnvironmentRecord"),
          "GlobalThisValue" -> NameT("Object"),
          "DeclarativeRecord" -> NameT("DeclarativeEnvironmentRecord"),
          "VarNames" -> ListT(StrT),
        ),
      ),

      // private environment record
      "PrivateEnvironmentRecord" -> TypeInfo(
        fields = Map(
          "OuterPrivateEnvironment" -> Set(
            NameT("PrivateEnvironmentRecord"),
            NullT,
          ),
          "Names" -> ListT(NameT("PrivateName")),
        ),
      ),

      // job callback record
      "JobCallbackRecord" -> TypeInfo(
        fields = Map(
          "Callback" -> NameT("FunctionObject"),
          "HostDefined" -> UndefT,
        ),
      ),

      // agent record
      "AgentRecord" -> TypeInfo(
        fields = Map(
          "LittleEndian" -> BoolT,
          "CanBlock" -> BoolT,
          "Signifier" -> NameT("AgentSignifier"),
          "IsLockFree1" -> BoolT,
          "IsLockFree2" -> BoolT,
          "IsLockFree8" -> BoolT,
          "CandidateExecution" -> NameT("CandidateExecutionRecord"),
          "KeptAlive" -> ListT(NameT("Object")),
        ),
      ),

      // script record
      "ScriptRecord" -> TypeInfo(
        fields = Map(
          "Realm" -> Set(NameT("RealmRecord"), UndefT),
          "ECMAScriptCode" -> AstT("Script"),
          "HostDefined" -> EMPTY,
        ),
      ),

      // module record
      "ModuleRecord" -> TypeInfo(
        fields = Map(
          "Realm" -> NameT("RealmRecord"),
          "Environment" -> Set(NameT("ModuleEnvironmentRecord"), EMPTY),
          "Namespace" -> Set(NameT("ModuleNamespaceExoticObject"), EMPTY),
          "HostDefined" -> UndefT,
        ),
      ),
      "CyclicModuleRecord" -> TypeInfo(
        parent = Some("ModuleRecord"),
        methods = Map(
          "Link" -> "CyclicModuleRecord.Link",
          "Evaluate" -> "CyclicModuleRecord.Evaluate",
        ),
        fields = Map(
          "Status" ->
          Set(
            UNLINKED,
            LINKING,
            LINKED,
            EVALUATING,
            EVALUATING_ASYNC,
            EVALUATED,
          ),
          "EvaluationError" -> Set(AbruptT, EMPTY),
          "DFSIndex" -> Set(MathT, EMPTY),
          "DFSAncestorIndex" -> Set(MathT, EMPTY),
          "RequestedModules" -> ListT(StrT),
          "CycleRoot" -> Set(NameT("CyclicModuleRecord"), EMPTY),
          "HasTLA" -> BoolT,
          "AsyncEvaluation" -> BoolT,
          "TopLevelCapability" -> Set(NameT("PromiseCapabilityRecord"), EMPTY),
          "AsyncParentModules" -> ListT(NameT("CyclicModuleRecord")),
          "PendingAsyncDependencies" -> Set(MathT, EMPTY),
        ),
      ),
      "SourceTextModuleRecord" -> TypeInfo(
        parent = Some("CyclicModuleRecord"),
        methods = Map(
          "GetExportedNames" -> "SourceTextModuleRecord.GetExportedNames",
          "ResolveExport" -> "SourceTextModuleRecord.ResolveExport",
          "InitializeEnvironment" -> "SourceTextModuleRecord.InitializeEnvironment",
          "ExecuteModule" -> "SourceTextModuleRecord.ExecuteModule",
        ),
        fields = Map(
          "ECMAScriptCode" -> AstT("Module"),
          "Context" -> NameT("ExecutionContext"),
          "ImportMeta" -> Set(NameT("Object"), EMPTY),
          "ImportEntries" -> ListT(NameT("ImportEntryRecord")),
          "LocalExportEntries" -> ListT(NameT("ExportEntryRecord")),
          "IndirectExportEntries" -> ListT(NameT("ExportEntryRecord")),
          "StarExportEntries" -> ListT(NameT("ExportEntryRecord")),
        ),
      ),
      "ImportEntryRecord" -> TypeInfo(
        fields = Map(
          "ModuleRequest" -> StrT,
          "ImportName" -> Set(StrT, NAMESPACE_OBJ),
          "LocalName" -> StrT,
        ),
      ),
      "ExportEntryRecord" -> TypeInfo(
        fields = Map(
          "ExportName" -> Set(StrT, NullT),
          "ModuleRequest" -> Set(StrT, NullT),
          "ImportName" -> Set(StrT, NullT, ALL, ALL_BUT_DEFAULT),
          "LocalName" -> Set(StrT, NullT),
        ),
      ),
      "ResolvedBindingRecord" -> TypeInfo(
        fields = Map(
          "Module" -> NameT("ModuleRecord"),
          "BindingName" -> Set(StrT, NAMESPACE),
        ),
      ),

      // symbol registry
      "GlobalSymbolRegistryRecord" -> TypeInfo(
        fields = Map(
          "Key" -> StrT,
          "Symbol" -> SymbolT,
        ),
      ),

      // match record
      "MatchRecord" -> TypeInfo(
        fields = Map(
          "StartIndex" -> MathT,
          "EndIndex" -> MathT,
        ),
      ),

      // pending job
      "PendingJob" -> TypeInfo(
        fields = Map(
          "Job" -> CloTopT,
          "Realm" -> NameT("RealmRecord"),
          "ScriptOrModule" ->
          Set(NameT("ScriptRecord"), NameT("ModuleRecord"), NullT),
        ),
      ),
    ),
  )
}

/** type information */
case class TypeInfo(
  parent: Option[String] = None,
  methods: Map[String, String] = Map(),
  fields: Map[String, Set[Type]] = Map(),
) {
  lazy val props: Map[String, Set[Type]] =
    val keys = methods.keySet ++ fields.keySet
    (for {
      k <- keys
      fs = fields.getOrElse(k, Set())
      tys = methods.get(k) match
        case None         => fs
        case Some(method) => fs + CloT(method)
    } yield k -> tys).toMap
}
