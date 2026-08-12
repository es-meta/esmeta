const AO__GeneratorStart = require("./GeneratorStart");
const AO__OrdinaryObjectCreate = require("./OrdinaryObjectCreate");

function AO__CreateIteratorFromClosure(closure, generatorBrand, generatorPrototype) {
  var extraSlots = arguments.length > 3 ? arguments[3] : [];
  var generator = AO__OrdinaryObjectCreate(generatorPrototype, extraSlots);
  generator["GeneratorBrand"] = generatorBrand;
  generator["GeneratorState"] = "suspended-start";
  // Skip 7~13
  var wrap = function(genContext) {globalThis["GeneratorContext"] = genContext; closure(genContext); globalThis["GeneratorContext"] = undefined;};
  AO__GeneratorStart(generator, wrap);
  return generator;
}

module.exports = AO__CreateIteratorFromClosure;