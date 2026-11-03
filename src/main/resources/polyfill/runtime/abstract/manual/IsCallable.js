// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-iscallable
function AO__IsCallable(argument) {
  "use strict";

  return typeof argument === "function";

  // 1. If argument is not an Object, return false.
  if (typeof argument !== "object") return false;

  // TODO: how to check internal method?
  // 2. If argument has a [[Call]] internal method, return true.
  if (typeof argument === "function") return true;

  // 3. Return false.
  return false;
}

module.exports = AO__IsCallable;
