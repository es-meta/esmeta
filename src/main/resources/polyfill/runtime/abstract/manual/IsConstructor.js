var AO__IsCallable = require("./IsCallable");

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-isconstructor
function AO__IsConstructor(argument) {
  if (!AO__IsCallable(argument)) return false;

  // try {
  //   Reflect.construct(function () { }, [], argument);
  // } catch {
  //   return false;
  // }
  return argument.prototype !== undefined
}

module.exports = AO__IsConstructor;
