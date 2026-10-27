var AO__IsArray = require("./IsArray");
var AO__IsConstructor = require("./IsConstructor");
var AO__IsObject = require("./IsObject");
var AO__ArrayCreate = require("./ArrayCreate");
var AO__Get = require("./Get");

// https://tc39.es/ecma262/multipage/ordinary-and-exotic-objects-behaviours.html#sec-arrayspeciescreate
function AO__ArraySpeciesCreate(originalArray, length) {
  "use strict";

  var ctor = function (originalArray) {
    var C;
    if (AO__IsArray(originalArray)) {
      C = originalArray.constructor;
      // cross-realm fallback
      if (AO__IsConstructor(C) && (C === Array || AO__IsArray(C.prototype)))
        C = undefined;
      else if (AO__IsObject(C)) {
        C = C[Symbol.species];
        if (C === null) C = undefined;
      }
    }
    return C === undefined ? Array : C;
  };
  return new (ctor(originalArray))(length === 0 ? 0 : length);

  // 1. Let isArray be ? IsArray(originalArray).
  var isArray = AO__IsArray(originalArray);
  // 2. If isArray is false, return ? ArrayCreate(length).
  if (isArray === false) return AO__ArrayCreate(length);
  // 3. Let C be ? Get(originalArray, "constructor").
  var C = AO__Get(originalArray, "constructor");
  // 4. If IsConstructor(C) is true, then
  if (AO__IsConstructor(C) === true) {
    // ??? Cross realm check
    if (C === Array || AO__IsArray(C.prototype)) C = undefined;
    // a. Let thisRealm be the current Realm Record.
    // b. Let realmC be ? GetFunctionRealm(C).
    // c. If thisRealm and realmC are not the same Realm Record, then
    //    i. If SameValue(C, realmC.[[Intrinsics]].[[%Array%]]) is true, set C to undefined.
  }
  // 5. If C is an Object, then
  if (typeof C === "object") {
    // a. Set C to ? Get(C, %Symbol.species%).
    C = AO__Get(C, Symbol.species);
    // b. If C is null, set C to undefined.
    if (C === null) C = undefined;
  }
  // 6. If C is undefined, return ? ArrayCreate(length).
  if (C === undefined) return AO__ArrayCreate(length);
  // 7. If IsConstructor(C) is false, throw a TypeError exception.
  if (AO__IsConstructor(C) === false) throw new TypeError();
  // 8. Return ? Construct(C, « 𝔽(length) »).
  return new C(length);
}

module.exports = AO__ArraySpeciesCreate;
