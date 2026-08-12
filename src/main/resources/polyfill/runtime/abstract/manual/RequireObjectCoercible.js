// 7.2 Testing and Comparison Operations
// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-requireobjectcoercible
function AO__RequireObjectCoercible(argument) {
  "use strict";

  if (argument === undefined || argument === null) throw new TypeError();

  return argument;
}

module.exports = AO__RequireObjectCoercible;
