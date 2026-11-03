var AO__IsCallable = require("./IsCallable");

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-isconstructor
function AO__IsConstructor(argument) {
  if (!AO__IsCallable(argument)) return false;

  // Using argument only as newTarget probes [[Construct]] without calling it.
  try {
    Reflect.construct(function () {}, [], argument);
  } catch (e) {
    return false;
  }
  return true;
}

module.exports = AO__IsConstructor;
