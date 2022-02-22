package esmeta.js.builtin

import esmeta.cfg.CFG
import esmeta.ir.TypeModel
import esmeta.interp.*
import esmeta.spec.*

/** model for intrinsics */
case class Intrinsics(spec: Spec) {

  /** shortcuts */
  private val T = true
  private val F = false
  private val U = Undef
  private def cfg = spec.program.cfg
  given CFG = cfg

  /** type model */
  // TODO refactoring
  private given Option[TypeModel] = Some(TypeModel(spec))

  /** get map for heap */
  lazy val map: Map[Addr, Obj] = {
    var _map = Map[Addr, Obj]()
    intrinsics.foreach(_map ++= _.toMap)
    _map
  }

  /** get intrinsic record */
  def obj: MapObj = MapObj("Record")((for {
    row <- spec.tables(WELL_KNOWN_INTRINSICS).rows
    intrKey <- row.headOption.map(_.trim.replace("%", ""))
  } yield Str(intrKey) -> intrAddr(intrKey)): _*)

  /** extensions for builtin model structure */
  // TODO refactoring
  extension (pair: (String, Struct)) {

    /** convert builtin model structure to heap objects */
    def toMap: Map[Addr, Obj] = {
      val (name, Struct(typeName, imap, nmap)) = pair
      var map = Map[Addr, Obj]()

      // base object
      map += intrAddr(name) -> MapObj(typeName)(
        (SUBMAP -> submapAddr(name) :: imap).map {
          case (k, v) => Str(k) -> v
        }: _*,
      )

      // submap object
      map ++= getSubmapObjects(name, nmap)

      map
    }
  }

  // get closures
  private def getClo(name: String): Clo =
    Clo(cfg.fnameMap(name), Map())

  // https://tc39.es/ecma262/#sec-ecmascript-standard-built-in-objects
  //
  // Unless specified otherwise, the [[Extensible]] internal slot of a built-in object initially has the value true
  // [[Extensible]] = true
  //
  // Every built-in function object has a [[Realm]] internal slot whose value is the Realm Record of the realm for which the object was initially created.
  // [[Realm]] = realm
  //
  // Unless otherwise specified every built-in function and every built-in constructor has the Function prototype object, which is the initial value of the expression Function.prototype (20.2.3), as the value of its [[Prototype]] internal slot.
  // For constructor, [[Prototype]] = Function.prototype
  //
  // Unless otherwise specified every built-in prototype object has the Object prototype object, which is the initial value of the expression Object.prototype (20.1.3), as the value of its [[Prototype]] internal slot, except the Object prototype object itself.
  // For prototype object, [[Prototype]] = Object.prototype
  //
  // Every built-in function object, including constructors, has a "length" property whose value is a non-negative integral Number. Unless otherwise specified, this value is equal to the number of required parameters shown in the subclause heading for the function description. Optional parameters and rest parameters are not included in the parameter count.
  // Unless otherwise specified, the "length" property of a built-in function object has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }.
  // .length = DataProperty { ... }
  //
  // Every built-in function object, including constructors, has a "name" property whose value is a String. Unless otherwise specified, this value is the name that is given to the function in this specification. Functions that are identified as anonymous functions use the empty String as the value of the "name" property. For functions that are specified as properties of objects, the name value is the property name string used to access the function. Functions that are specified as get or set accessor functions of built-in properties have "get" or "set" (respectively) passed to the prefix parameter when calling CreateBuiltinFunction.
  // The value of the "name" property is explicitly specified for each built-in functions whose property key is a Symbol value. If such an explicitly specified value starts with the prefix "get " or "set " and the function for which it is specified is a get or set accessor function of a built-in property, the value without the prefix is passed to the name parameter, and the value "get" or "set" (respectively) is passed to the prefix parameter when calling CreateBuiltinFunction.
  // Unless otherwise specified, the "name" property of a built-in function object has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }.
  // .name = DataProperty { ... }
  //
  // Every other data property described in clauses 19 through 28 and in Annex B.2 has the attributes { [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true } unless otherwise specified.
  // .data = DataProperty { ... }
  //
  // Every accessor property described in clauses 19 through 28 and in Annex B.2 has the attributes { [[Enumerable]]: false, [[Configurable]]: true } unless otherwise specified. If only a get accessor function is described, the set accessor function is the default value, undefined. If only a set accessor is described the get accessor is the default value, undefined.
  // .accessor = AccessorProperty { ... }

  // intrinsics
  private lazy val intrinsics: Map[String, Struct] = Map(
    "Function" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> getClo("Function"),
        "Construct" -> getClo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("Function"), F, F, T),
        "prototype" -> DataProperty(intrAddr("Function.prototype"), F, F, F),
      ),
    ),
    "Function.prototype" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
        "Code" -> getClo("Function.prototype"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(0.0), F, F, T),
        "name" -> DataProperty(Str(""), F, F, T),
        "constructor" -> DataProperty(intrAddr("Function"), T, F, T),
        "@@hasInstance" -> DataProperty(
          intrAddr("Function.prototype[@@hasInstance]"),
          F,
          F,
          F,
        ),
      ),
    ),
    "Object" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
        "Code" -> getClo("Object"),
        "Construct" -> getClo("BuiltinFunctionObject.Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("Object"), F, F, T),
        "prototype" -> DataProperty(intrAddr("Object.prototype"), F, F, F),
      ),
    ),
    "Object.prototype" -> Struct(
      typeName = "ImmutablePrototypeExoticObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> Null,
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Object"), T, F, T),
      ),
    ),
  )
}
