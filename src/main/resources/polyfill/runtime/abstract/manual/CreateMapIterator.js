var AO__RequireInternalSlot = require("./RequireInternalSlot");
var IteratorPrototype = require("../../internal/IteratorPrototype");

var MapIteratorPrototype = Object.create(IteratorPrototype);

Object.defineProperty(MapIteratorPrototype, "next", {
  value: function next() {
    var O = this;

    if (!O || typeof O !== "object" || !("_IteratedMap" in O)) {
      throw new TypeError(
        "Method Map Iterator.prototype.next called on incompatible receiver",
      );
    }

    var m = O._IteratedMap;
    var index = O._MapNextIndex;
    var itemKind = O._MapIterationKind;

    if (m === undefined) {
      return { value: undefined, done: true };
    }

    var entries = m["MapData"];
    while (index < entries.length) {
      var e = entries[index];

      index = index + 1;
      O._MapNextIndex = index;

      if (e["Key"] !== "empty") {
        var result;

        if (itemKind === "key") result = e["Key"];
        else if (itemKind === "value") result = e["Value"];
        else result = [e["Key"], e["Value"]];

        return { value: result, done: false };
      }
    }

    O._IteratedMap = undefined;
    return { value: undefined, done: true };
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

function CreateMapIterator(map, kind) {
  AO__RequireInternalSlot(map, "MapData");

  var iterator = Object.create(MapIteratorPrototype);
  iterator._IteratedMap = map;
  iterator._MapNextIndex = 0;
  iterator._MapIterationKind = kind;

  return iterator;
}

module.exports = CreateMapIterator;
