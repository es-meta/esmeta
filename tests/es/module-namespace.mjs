import * as ns from './module-namespace_FIXTURE.mjs';

globalThis.keys = Object.keys(ns).join();
globalThis.tag = ns[Symbol.toStringTag];
globalThis.extensible = Object.isExtensible(ns);
