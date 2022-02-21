package esmeta.js.builtin

import esmeta.ir.*
import esmeta.interp.*
import esmeta.spec.Spec

// TODO extract type model from spec
/** type modeling */
case class TypeModel(spec: Spec) {

  /** method map */
  type MethodMap = Map[String, Clo]

  /** get method map */
  def getMethods(tname: String): MethodMap = infos.get(tname) match {
    case Some(info) =>
      val parentMethods = info.parent.map(getMethods).getOrElse(Map())
      parentMethods ++ info.methods
    case None => Map()
  }

  /** type information */
  case class TypeInfo(
    parent: Option[String] = None,
    methods: MethodMap = Map(),
  )

  /** cfg */
  private def cfg = spec.program.cfg

  /** get closures */
  private def getClo(name: String): Clo = Clo(cfg.fnameMap(name), Map())

  lazy val infos: Map[String, TypeInfo] = Map(
    "Object" -> TypeInfo(
      parent = None,
      methods = Map(
        "GetPrototypeOf" -> getClo("OrdinaryObject.GetPrototypeOf"),
        "SetPrototypeOf" -> getClo("OrdinaryObject.SetPrototypeOf"),
        "IsExtensible" -> getClo("OrdinaryObject.IsExtensible"),
        "PreventExtensions" -> getClo("OrdinaryObject.PreventExtensions"),
        "GetOwnProperty" -> getClo("OrdinaryObject.GetOwnProperty"),
        "DefineOwnProperty" -> getClo("OrdinaryObject.DefineOwnProperty"),
        "HasProperty" -> getClo("OrdinaryObject.HasProperty"),
        "Get" -> getClo("OrdinaryObject.Get"),
        "Set" -> getClo("OrdinaryObject.Set"),
        "Delete" -> getClo("OrdinaryObject.Delete"),
        "OwnPropertyKeys" -> getClo("OrdinaryObject.OwnPropertyKeys"),
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
        "Call" -> getClo("ECMAScriptFunctionObject.Call"),
        "Construct" -> getClo("ECMAScriptFunctionObject.Construct"),
      ),
    ),
    "BuiltinFunctionObject" -> TypeInfo(
      parent = Some("FunctionObject"),
      methods = Map(
        "Call" -> getClo("BuiltinFunctionObject.Call"),
      ),
    ),
    "BoundFunctionExoticObject" -> TypeInfo(
      parent = Some("Object"),
      methods = Map(
        "Call" -> getClo("BoundFunctionExoticObject.Call"),
        "Construct" -> getClo("BoundFunctionExoticObject.Construct"),
      ),
    ),
    "ArrayExoticObject" -> TypeInfo(
      parent = Some("Object"),
      methods = Map(
        "DefineOwnProperty" -> getClo("ArrayExoticObject.DefineOwnProperty"),
      ),
    ),
    "StringExoticObject" -> TypeInfo(
      parent = Some("Object"),
      methods = Map(
        "GetOwnProperty" -> getClo("StringExoticObject.GetOwnProperty"),
        "DefineOwnProperty" -> getClo(
          "StringExoticObject.DefineOwnProperty",
        ),
        "OwnPropertyKeys" -> getClo("StringExoticObject.OwnPropertyKeys"),
      ),
    ),
    "ArgumentsExoticObject" -> TypeInfo(
      parent = Some("Object"),
      methods = Map(
        "GetOwnProperty" -> getClo("ArgumentsExoticObject.GetOwnProperty"),
        "DefineOwnProperty" -> getClo(
          "ArgumentsExoticObject.DefineOwnProperty",
        ),
        "Get" -> getClo("ArgumentsExoticObject.Get"),
        "Set" -> getClo("ArgumentsExoticObject.Set"),
        "Delete" -> getClo("ArgumentsExoticObject.Delete"),
      ),
    ),
    "IntegerIndexedExoticObject" -> TypeInfo(
      parent = Some("Object"),
      methods = Map(
        "GetOwnProperty" -> getClo(
          "IntegerIndexedExoticObject.GetOwnProperty",
        ),
        "HasProperty" -> getClo("IntegerIndexedExoticObject.HasProperty"),
        "DefineOwnProperty" -> getClo(
          "IntegerIndexedExoticObject.DefineOwnProperty",
        ),
        "Get" -> getClo("IntegerIndexedExoticObject.Get"),
        "Set" -> getClo("IntegerIndexedExoticObject.Set"),
        "Delete" -> getClo("IntegerIndexedExoticObject.Delete"),
        "OwnPropertyKeys" -> getClo(
          "IntegerIndexedExoticObject.OwnPropertyKeys",
        ),
      ),
    ),
    "ImmutablePrototypeExoticObject" -> TypeInfo(
      parent = Some("Object"),
      methods = Map(
        "SetPrototypeOf" -> getClo(
          "ImmutablePrototypeExoticObject.SetPrototypeOf",
        ),
      ),
    ),
    "ProxyExoticObject" -> TypeInfo(
      parent = Some("Object"),
      methods = Map(
        "GetPrototypeOf" -> getClo("ProxyExoticObject.GetPrototypeOf"),
        "SetPrototypeOf" -> getClo("ProxyExoticObject.SetPrototypeOf"),
        "IsExtensible" -> getClo("ProxyExoticObject.IsExtensible"),
        "PreventExtensions" -> getClo("ProxyExoticObject.PreventExtensions"),
        "GetOwnProperty" -> getClo("ProxyExoticObject.GetOwnProperty"),
        "DefineOwnProperty" -> getClo("ProxyExoticObject.DefineOwnProperty"),
        "HasProperty" -> getClo("ProxyExoticObject.HasProperty"),
        "Get" -> getClo("ProxyExoticObject.Get"),
        "Set" -> getClo("ProxyExoticObject.Set"),
        "Delete" -> getClo("ProxyExoticObject.Delete"),
        "OwnPropertyKeys" -> getClo("ProxyExoticObject.OwnPropertyKeys"),
        "Call" -> getClo("ProxyExoticObject.Call"),
        "Construct" -> getClo("ProxyExoticObject.Construct"),
      ),
    ),
    "ArrayBufferObject" -> TypeInfo(parent = Some("Object")),
    "BooleanObject" -> TypeInfo(parent = Some("OrdinaryObject")),
    "BigIntObject" -> TypeInfo(parent = Some("OrdinaryObject")),
    "NumberObject" -> TypeInfo(parent = Some("OrdinaryObject")),
    "SymbolObject" -> TypeInfo(parent = Some("OrdinaryObject")),
    // special instances
    "ForInIteratorInstance" -> TypeInfo(parent = Some("OrdinaryObject")),
    "AsynFromSyncIteratorInstance" -> TypeInfo(parent = Some("OrdinaryObject")),
    "PromiseInstance" -> TypeInfo(parent = Some("OrdinaryObject")),
    "GeneratorInstance" -> TypeInfo(parent = Some("OrdinaryObject")),
    "AsyncGeneratorInstance" -> TypeInfo(parent = Some("OrdinaryObject")),
    // environment records
    "LexicalEnvironment" -> TypeInfo(),
    "EnvironmentRecord" -> TypeInfo(parent = Some("LexicalEnvironment")),
    "DeclarativeEnvironmentRecord" -> TypeInfo(
      parent = Some("EnvironmentRecord"),
      methods = Map(
        "HasBinding" -> getClo("DeclarativeEnvironmentRecord.HasBinding"),
        "CreateMutableBinding" -> getClo(
          "DeclarativeEnvironmentRecord.CreateMutableBinding",
        ),
        "CreateImmutableBinding" -> getClo(
          "DeclarativeEnvironmentRecord.CreateImmutableBinding",
        ),
        "InitializeBinding" -> getClo(
          "DeclarativeEnvironmentRecord.InitializeBinding",
        ),
        "SetMutableBinding" -> getClo(
          "DeclarativeEnvironmentRecord.SetMutableBinding",
        ),
        "GetBindingValue" -> getClo(
          "DeclarativeEnvironmentRecord.GetBindingValue",
        ),
        "DeleteBinding" -> getClo(
          "DeclarativeEnvironmentRecord.DeleteBinding",
        ),
        "HasThisBinding" -> getClo(
          "DeclarativeEnvironmentRecord.HasThisBinding",
        ),
        "HasSuperBinding" -> getClo(
          "DeclarativeEnvironmentRecord.HasSuperBinding",
        ),
        "WithBaseObject" -> getClo(
          "DeclarativeEnvironmentRecord.WithBaseObject",
        ),
      ),
    ),
    "ObjectEnvironmentRecord" -> TypeInfo(
      parent = Some("EnvironmentRecord"),
      methods = Map(
        "HasBinding" -> getClo("ObjectEnvironmentRecord.HasBinding"),
        "CreateMutableBinding" -> getClo(
          "ObjectEnvironmentRecord.CreateMutableBinding",
        ),
        "InitializeBinding" -> getClo(
          "ObjectEnvironmentRecord.InitializeBinding",
        ),
        "SetMutableBinding" -> getClo(
          "ObjectEnvironmentRecord.SetMutableBinding",
        ),
        "GetBindingValue" -> getClo(
          "ObjectEnvironmentRecord.GetBindingValue",
        ),
        "DeleteBinding" -> getClo("ObjectEnvironmentRecord.DeleteBinding"),
        "HasThisBinding" -> getClo("ObjectEnvironmentRecord.HasThisBinding"),
        "HasSuperBinding" -> getClo(
          "ObjectEnvironmentRecord.HasSuperBinding",
        ),
        "WithBaseObject" -> getClo("ObjectEnvironmentRecord.WithBaseObject"),
      ),
    ),
    "FunctionEnvironmentRecord" -> TypeInfo(
      parent = Some("DeclarativeEnvironmentRecord"),
      methods = Map(
        "BindThisValue" -> getClo("FunctionEnvironmentRecord.BindThisValue"),
        "HasThisBinding" -> getClo(
          "FunctionEnvironmentRecord.HasThisBinding",
        ),
        "HasSuperBinding" -> getClo(
          "FunctionEnvironmentRecord.HasSuperBinding",
        ),
        "GetThisBinding" -> getClo(
          "FunctionEnvironmentRecord.GetThisBinding",
        ),
        "GetSuperBase" -> getClo("FunctionEnvironmentRecord.GetSuperBase"),
      ),
    ),
    "GlobalEnvironmentRecord" -> TypeInfo(
      parent = Some("EnvironmentRecord"),
      methods = Map(
        "HasBinding" -> getClo("GlobalEnvironmentRecord.HasBinding"),
        "CreateMutableBinding" -> getClo(
          "GlobalEnvironmentRecord.CreateMutableBinding",
        ),
        "CreateImmutableBinding" -> getClo(
          "GlobalEnvironmentRecord.CreateImmutableBinding",
        ),
        "InitializeBinding" -> getClo(
          "GlobalEnvironmentRecord.InitializeBinding",
        ),
        "SetMutableBinding" -> getClo(
          "GlobalEnvironmentRecord.SetMutableBinding",
        ),
        "GetBindingValue" -> getClo(
          "GlobalEnvironmentRecord.GetBindingValue",
        ),
        "DeleteBinding" -> getClo("GlobalEnvironmentRecord.DeleteBinding"),
        "HasThisBinding" -> getClo("GlobalEnvironmentRecord.HasThisBinding"),
        "HasSuperBinding" -> getClo(
          "GlobalEnvironmentRecord.HasSuperBinding",
        ),
        "WithBaseObject" -> getClo("GlobalEnvironmentRecord.WithBaseObject"),
        "GetThisBinding" -> getClo("GlobalEnvironmentRecord.GetThisBinding"),
        "HasVarDeclaration" -> getClo(
          "GlobalEnvironmentRecord.HasVarDeclaration",
        ),
        "HasLexicalDeclaration" -> getClo(
          "GlobalEnvironmentRecord.HasLexicalDeclaration",
        ),
        "HasRestrictedGlobalProperty" -> getClo(
          "GlobalEnvironmentRecord.HasRestrictedGlobalProperty",
        ),
        "CanDeclareGlobalVar" -> getClo(
          "GlobalEnvironmentRecord.CanDeclareGlobalVar",
        ),
        "CanDeclareGlobalFunction" -> getClo(
          "GlobalEnvironmentRecord.CanDeclareGlobalFunction",
        ),
        "CreateGlobalVarBinding" -> getClo(
          "GlobalEnvironmentRecord.CreateGlobalVarBinding",
        ),
        "CreateGlobalFunctionBinding" -> getClo(
          "GlobalEnvironmentRecord.CreateGlobalFunctionBinding",
        ),
      ),
    ),
  )
}
