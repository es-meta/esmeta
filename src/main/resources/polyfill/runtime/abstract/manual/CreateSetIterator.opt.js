var ENUM__key = require("../../internal/Enum")("key");
var ENUM__value = require("../../internal/Enum")("value");
var ENUM__key_value = require("../../internal/Enum")("key+value");
var IteratorPrototype = require("../../internal/IteratorPrototype");

// Like SetIterator.js, but walks the linked list of the optimized
// SetData. The iteration state is kept in a non-enumerable own property,
// which also brands the iterator for %SetIteratorPrototype%.next.
var STATE = "_SetIteratorState";
var hasOwn = Object.prototype.hasOwnProperty;

var SetIteratorPrototype = Object.create(IteratorPrototype);

Object.defineProperty(SetIteratorPrototype, "next", {
  value: function next() {
    var O = this;
    if (!O || typeof O !== "object" || !hasOwn.call(O, STATE)) {
      throw new TypeError(
        "Method Set Iterator.prototype.next called on incompatible receiver",
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
    var value = node.value;
    if (state.kind === ENUM__key_value)
      return { value: [value, value], done: false };
    return { value: value, done: false };
  },
  writable: true,
  enumerable: false,
  configurable: true,
});

Object.defineProperty(SetIteratorPrototype, Symbol.toStringTag, {
  value: "Set Iterator",
  writable: false,
  enumerable: false,
  configurable: true,
});

function CreateSetIterator(x, kind) {
  var data = x && hasOwn.call(x, "SetData") ? x["SetData"] : undefined;
  if (!data) throw new TypeError("CreateSetIterator: not a Set");

  var iterator = Object.create(SetIteratorPrototype);
  Object.defineProperty(iterator, STATE, {
    value: { data: data, kind: kind, last: null, finished: false },
    writable: false,
    enumerable: false,
    configurable: false,
  });
  return iterator;
}

module.exports = CreateSetIterator;
