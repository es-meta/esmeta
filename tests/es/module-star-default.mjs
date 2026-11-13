import * as ns from './module-star-default-a_FIXTURE.mjs';

globalThis.keys = Object.keys(ns).join();
import('./module-star-default-c_FIXTURE.mjs').catch(e => {
  globalThis.error = e.constructor.name;
});
