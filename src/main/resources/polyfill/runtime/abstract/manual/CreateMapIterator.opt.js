var ENUM__key = require("../../internal/Enum")("key");
var ENUM__value = require("../../internal/Enum")("value");
var ENUM__key_value = require("../../internal/Enum")("key+value");
var IteratorPrototype = require("../../internal/IteratorPrototype");

// Like MapIterator.js, but walks the linked list of the optimized
// MapData. The iteration state is kept in a non-enumerable own property,
// which also brands the iterator for %MapIteratorPrototype%.next.
var STATE = "_MapIteratorState";
var hasOwn = Object.prototype.hasOwnProperty;

var MapIteratorPrototype = Object.create(IteratorPrototype);

Object.defineProperty(MapIteratorPrototype, "next", {
  value: function next() {
    var O = this;
    if (!O || typeof O !== "object" || !hasOwn.call(O, STATE)) {
      throw new TypeError(
        "Method Map Iterator.prototype.next called on incompatible receiver",
      );
    }
    var state = O[STATE];
    if (state.finished) return { value: undefined, done: true };

    var anchor = state.last;
    while (anchor && anchor.removed) anchor = anchor.prev;
    var node = anchor ? anchor.next : state.data.head;
    while (node && node.removed) node = node.next;

    if (!node) {
      state.finished = true;
      state.last = null;
      return { value: undefined, done: true };
    }

    state.last = node;
    var key = node.Key;
    var value = node.Value;
    if (state.kind === ENUM__key) return { value: key, done: false };
    if (state.kind === ENUM__value) return { value: value, done: false };
    return { value: [key, value], done: false };
  },
  writable: true,
  enumerable: false,
  configurable: true,
});

Object.defineProperty(MapIteratorPrototype, Symbol.toStringTag, {
  value: "Map Iterator",
  writable: false,
  enumerable: false,
  configurable: true,
});

function CreateMapIterator(x, kind) {
  var data = x && hasOwn.call(x, "MapData") ? x["MapData"] : undefined;
  if (!data) throw new TypeError("CreateMapIterator: not a Map");

  var iterator = Object.create(MapIteratorPrototype);
  Object.defineProperty(iterator, STATE, {
    value: { data: data, kind: kind, last: null, finished: false },
    writable: false,
    enumerable: false,
    configurable: false,
  });
  return iterator;
}

module.exports = CreateMapIterator;
