var SetIteratorPrototype = {};

// If the environment has Symbols (or a polyfill), we make the iterator "iterable" 
// so it works in for..of loops if they are transpiled.
if (typeof Symbol !== "undefined" && Symbol.iterator) {
  Object.defineProperty(SetIteratorPrototype, Symbol.iterator, {
    value: function () { return this; },
    writable: true,
    enumerable: false,
    configurable: true
  });
}

// If the environment has toStringTag, compliant with Spec.
if (typeof Symbol !== "undefined" && Symbol.toStringTag) {
  Object.defineProperty(SetIteratorPrototype, Symbol.toStringTag, {
    value: "Set Iterator",
    writable: false,
    enumerable: false,
    configurable: true
  });
}

function CreateSetIterator(S, kind) {
  var data = S["SetData"];

  if (!data) throw new TypeError;
  var last = null;
  var finished = false;

  var iterator = Object.create(SetIteratorPrototype);

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
    var value = node.value;

    if (kind === "key+value") return { value: [value, value], done: false };
    return { value: value, done: false };
  };

  return iterator;
}

module.exports = CreateSetIterator;
