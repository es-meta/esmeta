// The global object. `globalThis` is ES2020, so engines without it get the
// global object as the `this` of a sloppy-mode function, as core-js does.
module.exports =
  typeof globalThis === "object" ? globalThis : Function("return this")();
