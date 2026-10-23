var AO__HasProperty = require("./HasProperty");
var AO__Get = require("./Get");

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-createdataproperty
function AO__CreateDataProperty(O, P, V) {
  "use strict";

  Object.defineProperty(O, P, {
    value: V,
    writable: true,
    enumerable: true,
    configurable: true
  });

  if (AO__HasProperty(O, "length") && AO__Get(O, "length") <= P) {
    O.length = P + 1;
  }

  return true;
}

module.exports = AO__CreateDataProperty;
