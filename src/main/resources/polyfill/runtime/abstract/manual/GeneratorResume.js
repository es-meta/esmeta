var ENUM__completed = require("../../internal/Enum")("completed");
var ENUM__executing = require("../../internal/Enum")("executing");
var AO__CreateIteratorResultObject = require('../CreateIteratorResultObject');
var AO__GeneratorValidate = require('./GeneratorValidate');
/*
var genContext = {
    Generator: generator,
    label: 0,
    signal: undefined,
    value: undefined,
    resumeInput: undefined,
    locals: {},
    body: undefined
  };
  */

function AO__GeneratorResume(generator, value, generatorBrand) {
  var state = AO__GeneratorValidate(generator, generatorBrand);
  if(state === ENUM__completed) return AO__CreateIteratorResultObject(undefined, true);
  // Skip Step 6, 7, 10 since it will be handled by engine
  generator["GeneratorState"] = ENUM__executing;
  // Step 9 
  // TODO input completion type (here it is fixed to Normal)
  var genContext = generator["GeneratorContext"];
  genContext["resumeInput"] = value;
  
  var result = genContext['body'](genContext);

  if(genContext.signal === "throw") throw result;
  return result;
}

module.exports = AO__GeneratorResume;