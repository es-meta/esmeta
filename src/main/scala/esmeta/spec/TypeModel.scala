package esmeta.spec

import scala.annotation.tailrec
import esmeta.analyzer.domain.* // TODO refactoring

/** type modeling */
case class TypeModel(infos: Map[String, TypeInfo] = Map()) {
  // TODO optimize

  /** get method map */
  def getMethod(tname: String): Map[String, String] = infos.get(tname) match {
    case Some(info) =>
      val parentMethods = info.parent.map(getMethod).getOrElse(Map())
      parentMethods ++ info.methods
    case None => Map()
  }

  /** get a method */
  def getMethod(tname: String, method: String): Option[String] = for {
    info <- infos.get(tname)
    fname <- info.methods
      .get(method)
      .orElse(info.parent.fold(None)(getMethod(_, method))),
  } yield fname

  /** get a property-types map */
  def getPropMap(tname: String): Map[String, Set[Type]] =
    infos.get(tname) match {
      case Some(info) =>
        val parentPropMap = info.parent.map(getPropMap).getOrElse(Map())
        parentPropMap ++ info.props
      case None => Map()
    }

  /** get types of property */
  def getProp(tname: String, p: String): Set[Type] =
    if (tname == "IntrinsicsRecord" && p.startsWith("%") && p.endsWith("%"))
      Set(NameT("Object"))
    else getPropMap(tname).getOrElse(p, ???)

  /** check subtype relation */
  def subType(t0: String, t1: String): Boolean =
    @tailrec
    def aux(tname: String, parents: List[String]): List[String] =
      infos.get(tname) match
        case Some(TypeInfo(Some(parent), _, _)) =>
          aux(parent, parent :: parents)
        case _ => parents
    aux(t0, List(t0)).contains(t1)
}
object TypeModel {

  /** conversion for type */
  given Conversion[Type, Set[Type]] = Set(_)

  // TODO extract type model from spec
  lazy val js: TypeModel = TypeModel(
    Map(
      // realm record
      "RealmRecord" -> TypeInfo(
        props = Map(
          "Intrinsics" -> NameT("IntrinsicsRecord"),
          "GlobalObject" -> Set(UndefT, NameT("OrdinaryObject")),
          "GlobalEnv" -> NameT("GlobalEnvironmentRecord"),
          "TemplateMap" -> ListT(NameT("TemplatePair")),
          "HostDefined" -> UndefT,
        ),
      ),
      "TemplatePair" -> TypeInfo(
        props = Map(
          "Site" -> AstT("TemplateLiteral"),
          "Array" -> NameT("Object"),
        ),
      ),

      // execution contexts
      "ExecutionContext" -> TypeInfo(
        props = Map(
          "Function" -> Set(NameT("FunctionObject"), NullT),
          "Realm" -> NameT("RealmRecord"),
          "ScriptOrModule" -> Set(NameT("ScriptRecord"), NameT("ModuleRecord")),
          "LexicalEnvironment" -> NameT("EnvironmentRecord"),
          "VariableEnvironment" -> NameT("EnvironmentRecord"),
          "PrivateEnvironment" -> Set(NameT("PrivateEnvironmentRecord"), NullT),
          "Generator" -> NameT("Object"),
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
      ),
      "OrdinaryObject" -> TypeInfo(
        parent = Some("Object"),
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
      ),
      "BuiltinFunctionObject" -> TypeInfo(
        parent = Some("FunctionObject"),
        methods = Map(
          "Call" -> "BuiltinFunctionObject.Call",
        ),
      ),
      "BoundFunctionExoticObject" -> TypeInfo(
        parent = Some("Object"),
        methods = Map(
          "Call" -> "BoundFunctionExoticObject.Call",
          "Construct" -> "BoundFunctionExoticObject.Construct",
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
      ),
      "ArrayBufferObject" -> TypeInfo(parent = Some("Object")),
      "BooleanObject" -> TypeInfo(parent = Some("OrdinaryObject")),
      "BigIntObject" -> TypeInfo(parent = Some("OrdinaryObject")),
      "NumberObject" -> TypeInfo(parent = Some("OrdinaryObject")),
      "SymbolObject" -> TypeInfo(parent = Some("OrdinaryObject")),

      // special instances
      "ForInIteratorInstance" -> TypeInfo(parent = Some("OrdinaryObject")),
      "AsynFromSyncIteratorInstance" -> TypeInfo(parent =
        Some("OrdinaryObject"),
      ),
      "PromiseInstance" -> TypeInfo(parent = Some("OrdinaryObject")),
      "GeneratorInstance" -> TypeInfo(parent = Some("OrdinaryObject")),
      "AsyncGeneratorInstance" -> TypeInfo(parent = Some("OrdinaryObject")),

      // environment records
      "LexicalEnvironment" -> TypeInfo(),
      "EnvironmentRecord" -> TypeInfo(parent = Some("LexicalEnvironment")),
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
      ),
    ),
  )
}

/** type information */
case class TypeInfo(
  parent: Option[String] = None,
  methods: Map[String, String] = Map(),
  props: Map[String, Set[Type]] = Map(),
)
