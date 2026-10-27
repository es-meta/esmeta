var ENUM__early_return = require("./Enum")("early-return");
var ENUM__empty = require("./Enum")("empty");
module.exports = function (setData, callback) {
  var thisSize = setData.length;
  var index = 0;
  while (index < thisSize) {
    var result = setData[index] === ENUM__empty ? undefined : callback(setData[index]);
    if (result && result["Type"] === ENUM__early_return) {
      return result;
    }
    index = index + 1;
    thisSize = setData.length;
  }
}
