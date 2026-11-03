var ENUM__early_return = require("./Enum")("early-return");
// Resume after the last visited node, as CreateSetIterator.opt.js does: if the
// callback removed it, rewind to its closest live predecessor first, so that
// elements added after the removal are still visited.
module.exports = function (setData, callback) {
  var last = null;
  while (true) {
    var anchor = last;
    while (anchor && anchor.removed) anchor = anchor.prev;
    var node = anchor ? anchor.next : setData.head;
    while (node && node.removed) node = node.next;
    if (!node) return;
    last = node;
    var result = callback(node.value);
    if (result !== undefined && result["Type"] === ENUM__early_return) {
      return result;
    }
  }
}
