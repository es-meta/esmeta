module.exports = function (mapData, callback) {
  var node = mapData.head;
  while (node) {
    if (!node.removed) callback(node);
    node = node.next;
  }
}