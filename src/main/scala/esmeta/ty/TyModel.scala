package esmeta.ty

import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec
import esmeta.analyzer.warning

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
  def isSubTy(l: String, r: String): Boolean =
    (l == r) || subTys.get(r).fold(false)(_ contains l)
  def isSubTy(l: String, rset: Set[String]): Boolean =
    rset.exists(r => isSubTy(l, r))
  def isSubTy(l: String, rset: BSet[String]): Boolean = rset match
    case Inf       => true
    case Fin(rset) => isSubTy(l, rset)
  def isSubTy(lset: Set[String], rset: Set[String]): Boolean =
    lset.forall(l => isSubTy(l, rset))

  /** loose subtyping relation between two value types */
  def isLooseSubTy(
    l: ValueTy,
    r: ValueTy,
  ): Boolean =
    val pureValue = isLooseSubTy(l.pureValue, r.pureValue)
    val normal = isLooseSubTy(l.normal, r.normal)
    val abrupt = l.abrupt <= r.abrupt
    pureValue && normal && abrupt

  /** loose subtyping relation between two pure value types */
  def isLooseSubTy(
    l: PureValueTy,
    r: PureValueTy,
  ): Boolean =
    val noName =
      ((l -- NameT.pureValue) <= (r -- NameT.pureValue))
    val name = ((l.name.set, r.name.set) match
      case (_, Inf) => true
      case (Inf, _) => false
      case (Fin(lset), Fin(rset)) =>
        lset.forall(l => rset.exists(r => isSubTy(l, r) || isSubTy(r, l)))
    )
    noName && name

  /** property map alias */
  type PropMap = Map[String, ValueTy]

  /** get types of property */
  def getPropOrElse(tname: String, p: String)(default: => ValueTy): ValueTy =
    if (tname == "IntrinsicsRecord" && p.startsWith("%") && p.endsWith("%"))
      // can be  made more precise for %ThrowTypeError% which is function object(https://tc39.es/ecma262/2022/#table-well-known-intrinsic-objects)
      NameT("Object")
    else
      propMap
        .getOrElse(tname, Map())
        .getOrElse(p, default)

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
        val newT = t || map.getOrElse(k, BotT)
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
        k -> (lt || rt)
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
        k -> (lt || rt)
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
          "Value" -> (ESValueT || AbsentT),
          "Writable" -> (BoolT || AbsentT),
          "Get" -> (NameT("FunctionObject") || UndefT || AbsentT),
          "Set" -> (NameT("FunctionObject") || UndefT || AbsentT),
          "Enumerable" -> (BoolT || AbsentT),
          "Configurable" -> (BoolT || AbsentT),
        ),
      ),

      // realm record
      "RealmRecord" -> TyInfo(
        fields = Map(
          "Intrinsics" -> NameT("IntrinsicsRecord"),
          "GlobalObject" -> (UndefT || NameT("Object")),
          "GlobalEnv" -> NameT("GlobalEnvironmentRecord"),
          "TemplateMap" -> ListT(NameT("TemplatePair")),
          "HostDefined" -> AnyT,
        ),
      ),
      "TemplatePair" -> TyInfo(
        fields = Map(
          "Site" -> AstT("TemplateLiteral"),
          "Array" -> NameT("Object"),
        ),
      ),

      // execution contexts
      "ExecutionContext" -> TyInfo(
        fields = Map(
          "Function" -> (NameT("FunctionObject") || NullT),
          "Realm" -> NameT("RealmRecord"),
          "ScriptOrModule" -> (NameT("ScriptRecord", "ModuleRecord") || NullT),
          "LexicalEnvironment" -> NameT("EnvironmentRecord"),
          "VariableEnvironment" -> NameT("EnvironmentRecord"),
          "PrivateEnvironment" -> (NameT("PrivateEnvironmentRecord") || NullT),
          "Generator" -> NameT("Object"),
        ),
      ),

      // reference record
      "ReferenceRecord" -> TyInfo(
        fields = Map(
          "Base" -> (
            ESPrimT ||
            NameT("Object", "EnvironmentRecord") ||
            UNRESOLVABLE,
          ),
          "ReferencedName" -> (StrT || SymbolT || NameT("PrivateName")),
          "Strict" -> BoolT,
          "ThisValue" -> (ESValueT || EMPTY),
        ),
      ),

      // private name
      "PrivateName" -> TyInfo(
        fields = Map("Description" -> StrT),
      ),

      // private element
      "PrivateElement" -> TyInfo(
        fields = Map(
          "Key" -> NameT("PrivateName"),
          "Kind" -> (FIELD || METHOD || ACCESSOR),
          "Value" -> (AbsentT || ESValueT),
          "Get" -> (NameT("FunctionObject") || UndefT || AbsentT),
          "Set" -> (NameT("FunctionObject") || UndefT || AbsentT),
        ),
      ),

      // class field definition record
      "ClassFieldDefinitionRecord" -> TyInfo(
        fields = Map(
          "Name" -> (NameT("PrivateName") || StrT || SymbolT),
          "Initializer" -> (NameT("FunctionObject") || EMPTY),
        ),
      ),

      // class static block definition record
      "ClassStaticBlockDefinitionRecord" -> TyInfo(
        fields = Map("BodyFunction" -> NameT("FunctionObject")),
      ),

      // iterator record
      "IteratorRecord" -> TyInfo(
        fields = Map(
          "Iterator" -> NameT("OrdinaryObject"),
          "NextMethod" -> NameT("FunctionObject"),
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
          "Prototype" -> (NameT("Object") || NullT),
          "SubMap" -> SubMapT(StrT || SymbolT, NameT("PropertyDescriptor")),
          "ErrorData" -> (AbsentT || UndefT),
          "BooleanData" -> (AbsentT || BoolT),
          "NumberData" -> (AbsentT || NumberT),
          "SymbolData" -> (AbsentT || SymbolT),
          "BigIntData" -> (AbsentT || BigIntT),
          "DateValue" -> (AbsentT || NumberT),
          "CleanupCallback" -> (AbsentT || NameT("JobCallbackRecord")),
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
        fields = Map(
          "Errors" -> (AbsentT || ListT),
          "Values" -> (AbsentT || ListT),
          "Index" -> (AbsentT || MathT),
        ),
      ),
      "ECMAScriptFunctionObject" -> TyInfo(
        parent = Some("FunctionObject"),
        methods = Map(
          "Call" -> "ECMAScriptFunctionObject.Call",
          "Construct" -> "ECMAScriptFunctionObject.Construct",
        ),
        fields = Map(
          "Environment" -> NameT("EnvironmentRecord"),
          "PrivateEnvironment" -> (
            NameT("PrivateEnvironmentRecord") ||
            NullT
          ),
          "FormalParameters" -> AstT,
          "ECMAScriptCode" -> AstT,
          "ConstructorKind" -> (BASE || DERIVED),
          "Realm" -> NameT("RealmRecord"),
          "ScriptOrModule" -> (
            NameT("ScriptRecord", "ModuleRecord") ||
            NullT
          ),
          "ThisMode" -> (LEXICAL || STRICT || GLOBAL),
          "Strict" -> BoolT,
          "HomeObject" -> (NameT("Object") || UndefT),
          "SourceText" -> StrT,
          "Fields" -> ListT(NameT("ClassFieldDefinitionRecord")),
          "PrivateMethods" -> ListT(NameT("PrivateElement")),
          "ClassFieldInitializerName" ->
          (StrT || SymbolT || NameT("PrivateName") || EMPTY),
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
          "Code" -> CloT,
          "Realm" -> NameT("RealmRecord"),
          "InitialName" -> (NullT || StrT),
        ),
      ),
      "BoundFunctionExoticObject" -> TyInfo(
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
        fields = Map("StringData" -> StrT),
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
          "ParameterMap" -> (NameT("OrdinaryObject") || NullT),
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
          "ViewedArrayBuffer" -> NameT("ArrayBufferObject"),
          "ArrayLength" -> MathT,
          "ByteOffset" -> MathT,
          "ContentTy" -> (NUMBER || BIGINT),
          "TydArrayName" -> StrT,
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
          "Module" -> NameT("ModuleRecord"),
          "Exports" -> ListT(StrT),
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
          "ProxyHandler" -> (NameT("Object") || NullT),
          "ProxyTarget" -> (NameT("Object") || NullT),
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
          "Object" -> NameT("Object"),
          "ObjectWasVisited" -> BoolT,
          "VisitedKeys" -> ListT(StrT),
          "RemainingKeys" -> ListT(StrT),
        ),
      ),
      "AsyncFromSyncIteratorInstance" -> TyInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "SyncIteratorRecord" -> NameT("IteratorRecord"),
        ),
      ),
      "PromiseCapabilityRecord" -> TyInfo(
        fields = Map(
          "Promise" -> NameT("Object"),
          "Resolve" -> NameT("FunctionObject"),
          "Reject" -> NameT("FunctionObject"),
        ),
      ),
      "PromiseReaction" -> TyInfo(
        fields = Map(
          "Capability" -> (NameT("PromiseCapabilityRecord") || UndefT),
          "Type" -> (FULFILL || REJECT),
          "Handler" -> (NameT("JobCallbackRecord") || EMPTY),
        ),
      ),
      "PromiseInstance" -> TyInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "PromiseState" -> (PENDING || FULFILLED || REJECTED),
          "PromiseResult" -> ESValueT,
          "PromiseFulfillReactions" -> ListT(NameT("PromiseReaction")),
          "PromiseRejectReactions" -> ListT(NameT("PromiseReaction")),
          "PromiseIsHandled" -> BoolT,
        ),
      ),
      "GeneratorInstance" -> TyInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "GeneratorState" -> (
            UndefT ||
            SUSPENDED_START ||
            SUSPENDED_YIELD ||
            EXECUTING ||
            COMPLETED
          ),
          "GeneratorContext" -> NameT("ExecutionContext"),
          "GeneratorBrand" -> (StrT || EMPTY),
        ),
      ),
      "AsyncGeneratorInstance" -> TyInfo(
        parent = Some("OrdinaryObject"),
        fields = Map(
          "AsyncGeneratorState" -> (
            UndefT ||
            SUSPENDED_START ||
            SUSPENDED_YIELD ||
            EXECUTING ||
            AWAITING_RETURN ||
            COMPLETED
          ),
          "AsyncGeneratorContext" -> NameT("ExecutionContext"),
          "AsyncGeneratorQueue" -> ListT(
            NameT("AsyncGeneratorRequestRecord"),
          ),
          "GeneratorBrand" -> (StrT || EMPTY),
        ),
      ),
      "AsyncGeneratorRequestRecord" -> TyInfo(
        fields = Map(
          // TODO "Completion" -> (NormalT(_) || AbruptT),
          "Capability" -> NameT("PromiseCapabilityRecord"),
        ),
      ),

      // environment records
      "EnvironmentRecord" -> TyInfo(
        fields = Map(
          "OuterEnv" -> (NullT || NameT("EnvironmentRecord")),
          "SubMap" -> SubMapT(StrT, NameT("Binding")),
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
          "ThisBindingStatus" -> (LEXICAL || INITIALIZED || UNINITIALIZED),
          "FunctionObject" -> NameT("FunctionObject"),
          "NewTarget" -> (NameT("Object") || UndefT),
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
          "BindingObject" -> NameT("Object"),
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
          "ObjectRecord" -> NameT("ObjectEnvironmentRecord"),
          "GlobalThisValue" -> NameT("Object"),
          "DeclarativeRecord" -> NameT("DeclarativeEnvironmentRecord"),
          "VarNames" -> ListT(StrT),
        ),
      ),

      // private environment record
      "PrivateEnvironmentRecord" -> TyInfo(
        fields = Map(
          "OuterPrivateEnvironment" -> (
            NameT("PrivateEnvironmentRecord") ||
            NullT
          ),
          "Names" -> ListT(NameT("PrivateName")),
        ),
      ),

      // job callback record
      "JobCallbackRecord" -> TyInfo(
        fields = Map(
          "Callback" -> NameT("FunctionObject"),
          "HostDefined" -> AnyT,
        ),
      ),

      // agent record
      "AgentRecord" -> TyInfo(
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
      "ScriptRecord" -> TyInfo(
        fields = Map(
          "Realm" -> (NameT("RealmRecord") || UndefT),
          "ECMAScriptCode" -> AstT("Script"),
          "HostDefined" -> AnyT,
        ),
      ),

      // module record
      "ModuleRecord" -> TyInfo(
        fields = Map(
          "Realm" -> NameT("RealmRecord"),
          "Environment" -> (NameT("ModuleEnvironmentRecord") || EMPTY),
          "Namespace" -> (NameT("ModuleNamespaceExoticObject") || EMPTY),
          "HostDefined" -> AnyT,
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
            UNLINKED ||
            LINKING ||
            LINKED ||
            EVALUATING ||
            EVALUATING_ASYNC ||
            EVALUATED
          ),
          "EvaluationError" -> (AbruptT || EMPTY),
          "DFSIndex" -> (MathT || EMPTY),
          "DFSAncestorIndex" -> (MathT || EMPTY),
          "RequestedModules" -> ListT(StrT),
          "CycleRoot" -> (NameT("CyclicModuleRecord") || EMPTY),
          "HasTLA" -> BoolT,
          "AsyncEvaluation" -> BoolT,
          "TopLevelCapability" -> (
            NameT("PromiseCapabilityRecord") ||
            EMPTY,
          ),
          "AsyncParentModules" -> ListT(NameT("CyclicModuleRecord")),
          "PendingAsyncDependencies" -> (MathT || EMPTY),
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
          "Context" -> NameT("ExecutionContext"),
          "ImportMeta" -> (NameT("Object") || EMPTY),
          "ImportEntries" -> ListT(NameT("ImportEntryRecord")),
          "LocalExportEntries" -> ListT(NameT("ExportEntryRecord")),
          "IndirectExportEntries" -> ListT(NameT("ExportEntryRecord")),
          "StarExportEntries" -> ListT(NameT("ExportEntryRecord")),
        ),
      ),
      "ImportEntryRecord" -> TyInfo(
        fields = Map(
          "ModuleRequest" -> StrT,
          "ImportName" -> (StrT || NAMESPACE_OBJ),
          "LocalName" -> StrT,
        ),
      ),
      "ExportEntryRecord" -> TyInfo(
        fields = Map(
          "ExportName" -> (StrT || NullT),
          "ModuleRequest" -> (StrT || NullT),
          "ImportName" -> (StrT || NullT || ALL || ALL_BUT_DEFAULT),
          "LocalName" -> (StrT || NullT),
        ),
      ),
      "ResolvedBindingRecord" -> TyInfo(
        fields = Map(
          "Module" -> NameT("ModuleRecord"),
          "BindingName" -> (StrT || NAMESPACE),
        ),
      ),

      // symbol registry
      "GlobalSymbolRegistryRecord" -> TyInfo(
        fields = Map(
          "Key" -> StrT,
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
          "Job" -> CloT,
          "Realm" -> NameT("RealmRecord"),
          "ScriptOrModule" ->
          (NameT("ScriptRecord", "ModuleRecord") || NullT),
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
        case Some(method) => fs || CloT(method)
    } yield k -> tys).toMap
}
