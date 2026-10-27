var IteratorPrototype = {};
Object.defineProperty(IteratorPrototype, Symbol.iterator, {
  value: function () {
    return this;
  },
  writable: true,
  enumerable: false,
  configurable: true,
});

module.exports = IteratorPrototype;
