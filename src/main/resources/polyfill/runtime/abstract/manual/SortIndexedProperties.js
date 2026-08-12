var AO__Get = require("./Get");
var AO__HasProperty = require("./HasProperty");
var AO__ToString = require("./ToString");

// https://tc39.es/ecma262/multipage/ordinary-and-exotic-objects-behaviours.html#sec-sortindexedproperties
function AO__SortIndexedProperties(obj, len, SortCompare, holes) {
  // 1. Let _items_ be a new empty List.
  var items = [];
  // 2-3. Read the indexed properties, optionally skipping holes.
  var k = 0;
  while (k < len) {
    var Pk = AO__ToString(k);
    var kRead = holes === "skip-holes" ? AO__HasProperty(obj, Pk) : true;
    if (kRead === true) items[items.length] = AO__Get(obj, Pk);
    k = k + 1;
  }
  // 4. Sort _items_ using an implementation-defined sequence of calls to
  //    _SortCompare_. The specification leaves the sequence open but requires
  //    the sort to be stable, so a merge sort is used rather than the host's
  //    Array.prototype.sort, which is only stable on newer engines. An abrupt
  //    completion from _SortCompare_ propagates and stops the remaining calls.
  // 5. Return _items_.
  return mergeSort(items, SortCompare);
}

function mergeSort(items, SortCompare) {
  if (items.length < 2) return items;
  var middle = Math.floor(items.length / 2);
  var left = mergeSort(items.slice(0, middle), SortCompare);
  var right = mergeSort(items.slice(middle), SortCompare);
  var merged = [];
  var i = 0;
  var j = 0;
  while (i < left.length && j < right.length) {
    // Keeping the left element on a tie is what makes the merge stable.
    if (SortCompare(left[i], right[j]) <= 0) merged[merged.length] = left[i++];
    else merged[merged.length] = right[j++];
  }
  while (i < left.length) merged[merged.length] = left[i++];
  while (j < right.length) merged[merged.length] = right[j++];
  return merged;
}

module.exports = AO__SortIndexedProperties;
