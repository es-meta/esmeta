var AO__CreateIteratorResultObject = require('../CreateIteratorResultObject');

function AO__GeneratorStart(generator, generatorBody) {
  var genContext = {
    Generator: generator,
    label: 0,
    signal: undefined,
    value: undefined,
    resumeInput: undefined,
    resumeInputType: undefined,
    locals: {},
    body: undefined
  }; // Step 2~3
  var closure = function _self() {
    genContext.signal = undefined;
    genContext.value = undefined;
    try {
      generatorBody(genContext);
    } catch(e) {
      genContext.signal = "throw";
      genContext.value = e;
    }
    if(genContext.signal === "yield") return genContext.value;
    genContext.Generator["GeneratorState"] = "completed";
    switch(genContext.signal) {
      case "return": var resultValue = genContext.value; break;
      case "throw": throw genContext.value;
      case "normal": 
      case undefined: var resultValue = undefined; break;
    }

    console.error(generator);
    return AO__CreateIteratorResultObject(resultValue, true);
  }
  genContext["body"] = closure;
  generator["GeneratorContext"] = genContext;
}

module.exports = AO__GeneratorStart;