var AO__CreateDataProperty = require("./CreateDataProperty");

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-definepropertyorthrow
function AO__CreateDataPropertyOrThrow(O, P, V) {
  "use strict";

  // 1. Let success be ? CreateDataProperty(O, P, V).
  var success = AO__CreateDataProperty(O, P, V);
  // 2. If success is false, throw a TypeError exception.
  if (success === false) throw new TypeError();
  // 3. Return unused.
}

module.exports = AO__CreateDataPropertyOrThrow;
