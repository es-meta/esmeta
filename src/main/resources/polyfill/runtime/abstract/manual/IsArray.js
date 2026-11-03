// The host's own Array.isArray is captured at load, before the polyfill has a
// chance to install its own -- that polyfill is written in terms of this very
// operation, so calling it through the global would come back here.
var nativeIsArray = Array.isArray;

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-isarray
function AO__IsArray(argument) {
  "use strict";

  // A Proxy forwards Object.prototype.toString to its target, so a bare
  // toString check misreports proxies -- test262 covers this, e.g.
  // built-ins/Array/prototype/flatMap/proxy-access-count.js. core-js takes the
  // same approach of deferring to the native operation where there is one.
  if (nativeIsArray) return nativeIsArray(argument);

  // 1. If argument is not an Object, return false.
  if (typeof argument !== "object" || argument === null) return false;
  // 2. If argument is an Array exotic object, return true.
  // 3. Proxies are transparent to JavaScript, so the spec's proxy case cannot
  //    be told apart here.
  // 4. Return false.
  return Object.prototype.toString.call(argument) === "[object Array]";
}

module.exports = AO__IsArray;
