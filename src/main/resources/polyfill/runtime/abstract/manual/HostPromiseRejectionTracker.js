// https://tc39.es/ecma262/#sec-host-promise-rejection-tracker
//
// The spec passes the operation as the String "reject" or "handle". Reports
// are deferred to a later task, like hosts do, so that a promise rejected and
// handled within the same job queue run is never reported.
var pending = [];
var reported = [];

function indexOf(list, x) {
  for (var i = 0; i < list.length; i++) if (list[i] === x) return i;
  return -1;
}

function flush() {
  var list = pending;
  pending = [];
  for (var i = 0; i < list.length; i++) {
    var promise = list[i];
    if (promise.PromiseIsHandled) continue;
    reported.push(promise);
    process.emit("unhandledRejection", promise.PromiseResult, promise);
  }
}

function AO__HostPromiseRejectionTracker(promise, operation) {
  if (typeof process === "undefined" || typeof process.emit !== "function")
    return;
  if (operation === "reject") {
    if (pending.length === 0 && typeof setTimeout === "function")
      setTimeout(flush, 0);
    pending.push(promise);
  } else if (operation === "handle") {
    var i = indexOf(pending, promise);
    if (i !== -1) {
      pending.splice(i, 1);
      return;
    }
    i = indexOf(reported, promise);
    if (i !== -1) {
      reported.splice(i, 1);
      process.emit("rejectionHandled", promise);
    }
  }
}

module.exports = AO__HostPromiseRejectionTracker;
