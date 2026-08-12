function AO_HostPromiseRejectionTracker(promise, operation) {
  if (typeof process === 'undefined') return;
  if(operation === 'reject') {
    process.emit('unhandledRejection', promise.PromiseResult, promise);
  } else if (operation === 'handle') {
    process.emit('rejectionHandled', promise);
  }
}

module.exports = AO_HostPromiseRejectionTracker;