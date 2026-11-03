var IN__Global = require("../../internal/Global");
var ENUM__suspended_start = require("../../internal/Enum")("suspended-start");
var AO__GeneratorStart = require("./GeneratorStart");
var AO__OrdinaryObjectCreate = require("./OrdinaryObjectCreate");

function AO__CreateIteratorFromClosure(closure, generatorBrand, generatorPrototype) {
  var extraSlots = arguments.length > 3 ? arguments[3] : [];
  var generator = AO__OrdinaryObjectCreate(generatorPrototype, extraSlots);
  generator["GeneratorBrand"] = generatorBrand;
  generator["GeneratorState"] = ENUM__suspended_start;
  // Skip 7~13
  var wrap = function(genContext) {IN__Global["GeneratorContext"] = genContext; closure(genContext); IN__Global["GeneratorContext"] = undefined;};
  AO__GeneratorStart(generator, wrap);
  return generator;
}

module.exports = AO__CreateIteratorFromClosure;