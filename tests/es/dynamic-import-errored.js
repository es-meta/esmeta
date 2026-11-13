var first, second;
import('./dynamic-import-errored-a_FIXTURE.mjs')
  .catch(e => {
    first = e;
    return import('./dynamic-import-errored-b_FIXTURE.mjs');
  })
  .catch(e => {
    second = e;
  });
