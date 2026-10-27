// Resume after the last visited node, as CreateMapIterator.opt.js does: if the
// callback removed it, rewind to its closest live predecessor first, so that
// entries added after the removal are still visited.
module.exports = function (mapData, callback) {
  var last = null;
  while (true) {
    var anchor = last;
    while (anchor && anchor.removed) anchor = anchor.prev;
    var node = anchor ? anchor.next : mapData.head;
    while (node && node.removed) node = node.next;
    if (!node) return;
    last = node;
    callback(node);
  }
}
