
var scheduler;
// Node v11
if (typeof queueMicrotask === 'function') {
  scheduler = queueMicrotask;
} 
// Node v11 >=
else if (typeof process !== 'undefined' && typeof process.nextTick === 'function') {
  scheduler = process.nextTick;
} 
// Fallback
else {
  scheduler = function(fn) { setTimeout(fn, 0); };
}

module.exports = function(job, realm) {
  scheduler(job);
};