// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-definepropertyorthrow
var FIELDS = [
  ["Value", "value"],
  ["Writable", "writable"],
  ["Get", "get"],
  ["Set", "set"],
  ["Enumerable", "enumerable"],
  ["Configurable", "configurable"],
];

function AO__DefinePropertyOrThrow(O, P, desc) {
  "use strict";

  // a spec Property Descriptor uses capitalized field names
  var jsDesc = {};
  for (var i = 0; i < FIELDS.length; i++) {
    if (Object.prototype.hasOwnProperty.call(desc, FIELDS[i][0]))
      jsDesc[FIELDS[i][1]] = desc[FIELDS[i][0]];
  }

  // 1. Let success be ? O.[[DefineOwnProperty]](P, desc).
  // 2. If success is false, throw a TypeError exception.
  // (`Object.defineProperty` throws a TypeError itself when it fails.)
  Object.defineProperty(O, P, jsDesc);
  // 3. Return unused.
}

module.exports = AO__DefinePropertyOrThrow;
