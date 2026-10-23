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

function AO__GeneratorYield(iteratorResult) {
  var genContext = globalThis["GeneratorContext"];
  var generator = genContext["Generator"];
  generator["GeneratorState"] = "suspended-yield";

  genContext["signal"] = "yield";
  genContext["value"] = iteratorResult;

  return genContext["value"];
}

module.exports = AO__GeneratorYield;