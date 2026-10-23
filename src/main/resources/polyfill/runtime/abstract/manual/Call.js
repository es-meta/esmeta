var AO__IsCallable = require("./IsCallable");

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-call
function AO__Call(F, V) {
  "use strict";

  var argumentsList = arguments[2];

  // 1. If argumentsList is not present, set argumentsList to a new empty List.
  if (argumentsList === undefined) argumentsList = [];

  // 2. If IsCallable(F) is false, throw a TypeError exception.
  if (AO__IsCallable(F) === false)
    throw new TypeError("AO__Call : F is not callable");

  // 3. Return ? F.[[Call]](V, argumentsList).
  return F.call(V, ...argumentsList);
}

module.exports = AO__Call;
