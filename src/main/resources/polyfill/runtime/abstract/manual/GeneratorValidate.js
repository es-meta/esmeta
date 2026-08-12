var AO__RequireInternalSlot = require('../RequireInternalSlot');

function AO__GeneratorValidate(generator, generatorBrand) {
  AO__RequireInternalSlot(generator, "GeneratorState");
  AO__RequireInternalSlot(generator, "GeneratorBrand");
  if(generator["GeneratorBrand"] !== generatorBrand) throw new TypeError('generatorBrand unmatch');
  var state = generator["GeneratorState"];
  if(state === "executing") throw new TypeError('generator validation failed: it is executing');
  return state;
}

module.exports = AO__GeneratorValidate;