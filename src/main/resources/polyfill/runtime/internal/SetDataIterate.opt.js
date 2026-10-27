var ENUM__early_return = require("./Enum")("early-return");
module.exports = function (setData, callback) {
  var node = setData.head;
  while (node) {
    var e = node.value;
    if (!node.removed) {
      var result = callback(e);
      if (result !== undefined && result["Type"] === ENUM__early_return) {
        return result;
      }
    }
    node = node.next;
  }
}
