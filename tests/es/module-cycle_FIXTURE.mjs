import { a } from './module-cycle.mjs';
export const b = 'b';
export const order = ['fixture'];
export function getA() {
  return a;
}
