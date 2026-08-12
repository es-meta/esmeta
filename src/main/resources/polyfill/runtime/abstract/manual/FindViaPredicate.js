var AO__IsCallable = require("./IsCallable");
var AO__ToString = require("./ToString");
var AO__Get = require("./Get");
var AO__Call = require("./Call");
// ToBoolean is generated, so it sits one level up rather than beside this file.
var AO__ToBoolean = require("../ToBoolean");

// 23.1.3 Properties of the Array Prototype Object
// https://tc39.es/ecma262/multipage/indexed-collections.html#sec-findviapredicate
function AO__FindViaPredicate(O, len, direction, predicate, thisArg) {
  // 1. If IsCallable(predicate) is false, throw a TypeError exception.
  if (AO__IsCallable(predicate) === false) throw new TypeError();

  // 2. If direction is ascending, then
  if (direction === "ascending") {
    // a. Let indices be a List of the integers in the interval from 0 (inclusive) to len (exclusive), in ascending order.
    // var indices = Array.from({ length: len }, function (v, k) { return k; });
  }
  // 3. Else,
  else {
    // a. Let indices be a List of the integers in the interval from 0 (inclusive) to len (exclusive), in descending order.
    // var indices = Array.from({ length: len }, function (v, k) { return len - k - 1; });
  }
  // test262/test/built-ins/Array/prototype/findLast/maximum-index.js
  // indicies가 index를 가지는 Array로 구현하면 안됨

  // 4. For each integer k of indices, do
  for (var _k = 0; _k < len; _k++) {
    var k = direction === "ascending" ? _k : len - _k - 1;
    // a. Let Pk be ! ToString(𝔽(k)).
    var Pk = AO__ToString(k);
    // b. NOTE: If O is a TypedArray, the following invocation of Get will return a normal completion.
    // c. Let kValue be ? Get(O, Pk).
    var kValue = AO__Get(O, Pk);
    // d. Let testResult be ? Call(predicate, thisArg, « kValue, 𝔽(k), O »).
    var testResult = AO__Call(predicate, thisArg, [kValue, k, O]);
    // e. If ToBoolean(testResult) is true, return the Record { [[Index]]: 𝔽(k), [[Value]]: kValue }.
    if (AO__ToBoolean(testResult) === true) return { Index: k, Value: kValue };
  }
  // 5. Return the Record { [[Index]]: -1𝔽, [[Value]]: undefined }.
  return { Index: -1, Value: undefined };
}

module.exports = AO__FindViaPredicate;
