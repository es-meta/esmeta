var AO__ToString = require("./ToString");
var AO__ArrayCreate = require("./ArrayCreate");
var AO__CreateDataPropertyOrThrow = require("./CreateDataPropertyOrThrow");

function AO__CreateArrayFromList(elements) {
  // 1. Let array be ! ArrayCreate(0).
  let array = AO__ArrayCreate(0);
  // 1. Let n be 0.
  let n = 0;
  // 1. For each element e of elements, do
  for (var i = 0; i < elements.length; i++) {
    var e = elements[i];
    // 1. Perform ! CreateDataPropertyOrThrow(array, ! ToString(𝔽(n)), e).
    AO__CreateDataPropertyOrThrow(array, AO__ToString(n), e);
    // 1. Set n to n + 1.
    n = n + 1;
  }
  // 1. Return array.
  return array;
}

module.exports = AO__CreateArrayFromList;
