import * as self from './module-self-import.mjs';
export const x = 'x';

globalThis.result = self.x;
