var $ObjectCreate = Object.create;
module.exports = function () {
  return { index: $ObjectCreate(null), head: null, tail: null, size: 0, frozen: [] };
}
