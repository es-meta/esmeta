"use strict";

var AO__IsObject = require("./IsObject");

module.exports = function (v) {
// 1. If v is an Object, return true.
  // We check if it's an object (excluding null) or a function.
  if (AO__IsObject(v)) {
    return true;
  }

  // 2. If v is a Symbol and KeyForSymbol(v) is undefined, return true.
  // Symbol.keyFor() retrieves the key for a symbol from the global registry.
  // It returns undefined for local symbols created with Symbol().
  if (typeof v === 'symbol' && Symbol.keyFor(v) === undefined) {
    return true;
  }

  // 3. Return false.
  return false;
};
