var syntaxError, notFound;
import('./dynamic-import-error_FIXTURE.mjs').catch(e => {
  syntaxError = e.constructor.name;
});
import('./no-such-module.mjs').catch(e => {
  notFound = e.constructor.name;
});
