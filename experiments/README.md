```sh
uv run collect_test262_missed_fuzz_covered.py -o report
uv run run_samples.py
```

### Notes
- `collect_test262_missed_fuzz_covered.py`
  - Make sure the detailed Test262 logs and the fuzz logs exist before running the script.
  - Filters Fuzz `branch-coverage.json` using branch IDs listed in Test262 `target-conds.json`.
  - For each remaining entry, reads `minimal/script.js` and converts the JS source into a single line (whitespace-normalized).
  - Keeps only branches where Fuzz covers both T and F, then annotates which side Test262 covers (true only / false only / neither) using Test262 `branch-coverage.json`.
  - Use `-o` option to choose the output file (default: stdout).

- run_samples.py
  - Make a results for mutating samples until they cover flipped case.