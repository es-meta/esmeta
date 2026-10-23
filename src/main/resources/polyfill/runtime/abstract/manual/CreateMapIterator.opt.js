var MapIteratorPrototype = {};

// If the environment has Symbols (or a polyfill), we make the iterator "iterable" 
// so it works in for..of loops if they are transpiled.
if (typeof Symbol !== "undefined" && Symbol.iterator) {
  Object.defineProperty(MapIteratorPrototype, Symbol.iterator, {
    value: function () { return this; },
    writable: true,
    enumerable: false,
    configurable: true
  });
}

// If the environment has toStringTag, compliant with Spec.
if (typeof Symbol !== "undefined" && Symbol.toStringTag) {
  Object.defineProperty(MapIteratorPrototype, Symbol.toStringTag, {
    value: "Map Iterator",
    writable: false,
    enumerable: false,
    configurable: true
  });
}

function CreateMapIterator(M, kind) {
  var data = M["MapData"];

  if (!data) throw new TypeError;
  var last = null;
  var finished = false;
  var iterator = Object.create(MapIteratorPrototype);

  iterator.next = function () {
    if (finished) return { value: undefined, done: true };

    var anchor = last;
    while (anchor && anchor.removed) anchor = anchor.prev;
    var node = anchor ? anchor.next : data.head;
    while (node && node.removed) node = node.next;

    if (!node) {
      finished = true;
      last = null;
      return { value: undefined, done: true };
    }

    last = node;
    var key = node.Key;
    var value = node.Value;

    if (kind === "key") return { value: key, done: false };
    if (kind === "value") return { value: value, done: false };

    return { value: [key, value], done: false };
  };

  return iterator;
}

module.exports = CreateMapIterator;
