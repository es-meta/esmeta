Object.freeze.call(undefined, (() => { const buffer = new ArrayBuffer(8); const value = new Float64Array(buffer); buffer.transfer(); return value; })());
