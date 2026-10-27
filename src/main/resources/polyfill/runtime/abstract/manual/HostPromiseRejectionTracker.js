var ENUM__handle = require("../../internal/Enum")("handle");
var ENUM__reject = require("../../internal/Enum")("reject");
function AO__HostPromiseRejectionTracker(promise, operation) {
  if (typeof process === 'undefined') return;
  if(operation === ENUM__reject) {
    process.emit('unhandledRejection', promise.PromiseResult, promise);
  } else if (operation === ENUM__handle) {
    process.emit('rejectionHandled', promise);
  }
}

module.exports = AO__HostPromiseRejectionTracker;