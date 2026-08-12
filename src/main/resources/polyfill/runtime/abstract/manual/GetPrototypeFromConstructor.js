var AO__Get = require("./Get");
var AO__IsObject = require("./IsObject");
// var AO__GetFunctionRealm = require("./GetFunctionRealm");

function AO__GetPrototypeFromConstructor(constructor, intrinsicDefaultProto) {
  // 1. Assert: _intrinsicDefaultProto_ is this specification's name of an intrinsic object. The corresponding object must be an intrinsic that is intended to be used as the [[Prototype]] value of an object.
  // 1. Let _proto_ be ? Get(_constructor_, *"prototype"*).
  var proto = AO__Get(constructor, "prototype");
  // 1. If _proto_ is not an Object, then
  if (!AO__IsObject(proto)) {
    // 1. Let _realm_ be ? GetFunctionRealm(_constructor_).
    // var realm = AO__GetFunctionRealm(constructor);
    // 1. Set _proto_ to _realm_'s intrinsic object named _intrinsicDefaultProto_.
    // proto = realm[intrinsicDefaultProto];
  }
  // 1. Return _proto_.
  return proto;
}

module.exports = AO__GetPrototypeFromConstructor;
