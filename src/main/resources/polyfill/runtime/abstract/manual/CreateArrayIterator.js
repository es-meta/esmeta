var ENUM__empty = require("../../internal/Enum")("empty");
var ENUM__key = require("../../internal/Enum")("key");
var ENUM__value = require("../../internal/Enum")("value");
var AO__Get = require("./Get");
var AO__LengthOfArrayLike = require("../LengthOfArrayLike");
var AO__ToString = require("./ToString");
var AO__CreateArrayFromList = require("../CreateArrayFromList");
var AO__GeneratorYield = require("./GeneratorYield");
var CreateIteratorResultObject = require("../CreateIteratorResultObject");
var AO__CreateIteratorFromClosure = require("./CreateIteratorFromClosure");
var AO__GeneratorResume = require("./GeneratorResume");
var IteratorPrototype = require("../../internal/IteratorPrototype");

var ArrayIteratorPrototype = Object.create(IteratorPrototype);

Object.defineProperty(ArrayIteratorPrototype, "next", {
  value: function next() {
    return AO__GeneratorResume(this, ENUM__empty, "%ArrayIteratorPrototype%");
  },
  writable: true, enumerable: false, configurable: true,
});

Object.defineProperty(ArrayIteratorPrototype, Symbol.toStringTag, {
  value: "Array Iterator",
  writable: false, enumerable: false, configurable: true,
});

function AO__CreateArrayIterator(array, kind) {
  var closure = function _self(genContext) {
    genContext["signal"] = undefined;
    genContext["value"] = undefined;
    while(true) {
      switch(genContext.label) {
        case 0:
          genContext.locals.index = 0;
          genContext.label = 1;
          break;
        case 1:
          genContext.locals.len = AO__LengthOfArrayLike(array);
          if(genContext.locals.index >= genContext.locals.len) {
            genContext.label = -1;
            return undefined;
          }
          genContext.locals.indexNumber = genContext.locals.index;
          if(kind === ENUM__key) genContext.locals.result = genContext.locals.indexNumber;
          else {
            genContext.locals.elementKey = AO__ToString(genContext.locals.indexNumber);
            genContext.locals.elementValue = AO__Get(array, genContext.locals.elementKey);
            if(kind === ENUM__value) genContext.locals.result = genContext.locals.elementValue;
            else genContext.locals.result = AO__CreateArrayFromList([genContext.locals.indexNumber, genContext.locals.elementValue]);
          }
          genContext.locals.index = genContext.locals.index + 1;
          genContext.label = 1;

          return AO__GeneratorYield(CreateIteratorResultObject(genContext.locals.result, false));
        case -1: // Generator return
          return undefined;
        default: 
          throw Error("Unreachable label: " + genContext.label);
      }
    }
  }
  return AO__CreateIteratorFromClosure(closure, "%ArrayIteratorPrototype%", ArrayIteratorPrototype);
}

module.exports = AO__CreateArrayIterator;