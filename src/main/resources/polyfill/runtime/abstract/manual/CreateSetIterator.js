var AO__RequireInternalSlot = require("./RequireInternalSlot");
var IteratorPrototype = require("../../internal/IteratorPrototype");

// sec-%setiteratorprototype%-object
// <li>has properties that are inherited by all Set Iterator objects.</li>
// <li>is an ordinary object.</li>
// <li>has a [[Prototype]] internal slot whose value is %Iterator.prototype%.</li>
// <li>has the following properties:</li>
var SetIteratorPrototype = Object.create(IteratorPrototype);

// sec-%setiteratorprototype%.next
Object.defineProperty(SetIteratorPrototype, "next", {
  value: function next() {
    // 1. Return ? <emu-meta suppress-effects="user-code">GeneratorResume(*this* value, ~empty~, *"%SetIteratorPrototype%"*)</emu-meta>.

    // -- ai generated / need review --
    var O = this;
    if (!O || typeof O !== "object" || !("_IteratedSet" in O)) {
      throw new TypeError(
        "Method Set Iterator.prototype.next called on incompatible receiver",
      );
    }

    var s = O._IteratedSet;
    var index = O._SetNextIndex;
    var itemKind = O._SetIterationKind;

    if (s === undefined) {
      return { value: undefined, done: true };
    }

    // 3. Loop through SetData
    var entries = s["SetData"];
    while (index < entries.length) {
      var e = entries[index];
      index = index + 1;

      // Update the index on the iterator
      O._SetNextIndex = index;

      // 4. If element is not empty, return it
      if (e !== "empty") {
        if (itemKind === "key+value") {
          return { value: [e, e], done: false };
        }
        return { value: e, done: false };
      }
    }

    // 5. Mark as finished
    O._IteratedSet = undefined;
    return { value: undefined, done: true };
  },
  writable: true,
  enumerable: false,
  configurable: true,
});

// sec-%setiteratorprototype%-%symbol.tostringtag%
// <h1>%SetIteratorPrototype% [ %Symbol.toStringTag% ]</h1>
// <p>The initial value of the %Symbol.toStringTag% property is the String value *"Set Iterator"*.</p>
// <p>This property has the attributes { [[Writable]]: *false*, [[Enumerable]]: *false*, [[Configurable]]: *true* }.</p>
Object.defineProperty(SetIteratorPrototype, Symbol.toStringTag, {
  value: "Set Iterator",
  writable: false,
  enumerable: false,
  configurable: true,
});

function CreateSetIterator(set, kind) {
  AO__RequireInternalSlot(set, "SetData");

  var iterator = Object.create(SetIteratorPrototype);
  iterator._IteratedSet = set;
  iterator._SetNextIndex = 0;
  iterator._SetIterationKind = kind;

  return iterator;
}

module.exports = CreateSetIterator;
