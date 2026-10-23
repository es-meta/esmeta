module.exports = function (setData, callback) {
  var node = setData.head;
  while (node) {
    var e = node.value;
    if (!node.removed) {
      var result = callback(e);
      if (result !== undefined && result["Type"] === "early-return") {
        return result;
      }
    }
    node = node.next;
  }
}
