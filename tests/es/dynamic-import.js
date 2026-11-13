var result;
import('./dynamic-import_FIXTURE.mjs').then(ns => {
  result = ns.default;
});
