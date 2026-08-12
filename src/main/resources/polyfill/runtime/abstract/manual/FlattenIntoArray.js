var AO__ToString = require("./ToString");
var AO__HasProperty = require("./HasProperty");
var AO__Get = require("./Get");
var AO__Call = require("./Call");
var AO__IsArray = require("./IsArray");
var AO__LengthOfArrayLike = require("./LengthOfArrayLike");
var AO__CreateDataPropertyOrThrow = require("./CreateDataPropertyOrThrow");

// https://tc39.es/ecma262/multipage/indexed-collections.html#sec-flattenintoarray
function AO__FlattenIntoArray(target, source, sourceLen, start, depth) {
  "use strict";

  var mapperFunction = arguments[5];
  var mapperFunctionIsPresent = arguments.length > 5;

  var thisArg = arguments[6];
  var thisArgIsPresent = arguments.length > 6;

  // 1. Assert: If mapperFunction is present, then IsCallable(mapperFunction) is true, thisArg is present, and depth is 1.
  // 2. Let targetIndex be start.
  var targetIndex = start;
  // 3. Let sourceIndex be +0𝔽.
  var sourceIndex = 0;
  // 4. Repeat, while ℝ(sourceIndex) < sourceLen,
  while (sourceIndex < sourceLen) {
    // a. Let P be ! ToString(sourceIndex).
    var P = AO__ToString(sourceIndex);
    // b. Let exists be ? HasProperty(source, P).
    var exists = AO__HasProperty(source, P);
    // c. If exists is true, then
    if (exists === true) {
      // i. Let element be ? Get(source, P).
      var element = AO__Get(source, P);
      // ii. If mapperFunction is present, then
      if (mapperFunctionIsPresent) {
        // 1. Set element to ? Call(mapperFunction, thisArg, « element, sourceIndex, source »).
        element = AO__Call(mapperFunction, thisArg, [
          element,
          sourceIndex,
          source,
        ]);
      }
      // iii. Let shouldFlatten be false.
      var shouldFlatten = false;
      // iv. If depth > 0, then
      if (depth > 0) {
        // 1. Set shouldFlatten to ? IsArray(element).
        shouldFlatten = AO__IsArray(element);
      }
      // v. If shouldFlatten is true, then
      if (shouldFlatten === true) {
        // 1. If depth = +∞, let newDepth be +∞.
        if (depth === Infinity) var newDepth = Infinity;
        // 2. Else, let newDepth be depth - 1.
        else var newDepth = depth - 1;
        // 3. Let elementLen be ? LengthOfArrayLike(element).
        var elementLen = AO__LengthOfArrayLike(element);
        // 4. Set targetIndex to ? FlattenIntoArray(target, element, elementLen, targetIndex, newDepth).
        targetIndex = AO__FlattenIntoArray(
          target,
          element,
          elementLen,
          targetIndex,
          newDepth,
        );
      }
      // vi. Else,
      else {
        // 1. If targetIndex ≥ 2**53 - 1, throw a TypeError exception.
        if (targetIndex >= Math.pow(2, 53) - 1) throw new TypeError();
        // 2. Perform ? CreateDataPropertyOrThrow(target, ! ToString(𝔽(targetIndex)), element).
        AO__CreateDataPropertyOrThrow(
          target,
          AO__ToString(targetIndex),
          element,
        );
        // 3. Set targetIndex to targetIndex + 1.
        targetIndex = targetIndex + 1;
      }
    }
    // d. Set sourceIndex to sourceIndex + 1𝔽.
    sourceIndex = sourceIndex + 1;
  }
  // 5. Return targetIndex.
  return targetIndex;
}

module.exports = AO__FlattenIntoArray;
