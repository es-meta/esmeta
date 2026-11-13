import { count, increment, default as name } from './module-binding_FIXTURE.mjs';

var before = count;
increment();
globalThis.result = [before, count, name].join();
