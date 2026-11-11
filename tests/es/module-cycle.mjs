import { b, order, getA } from './module-cycle_FIXTURE.mjs';
export const a = 'a';

order.push('main');
globalThis.result = order.join() + ':' + b + getA();
