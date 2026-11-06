## Submission

This is a new submission.

## Test environments

<!-- Replace with the results of the runs listed in RELEASING.md before
     submitting. -->

## R CMD check results

<!-- Paste the summary line from each run, e.g. 0 errors | 0 warnings | 1 note -->

* The expected NOTE on a first submission is "New submission".

## Notes for the reviewer

* Pattern matching and rewriting never evaluate user code. `nitro()`
  evaluates the user's code only when `benchmark = TRUE` and a rewrite was
  applied; each evaluation happens in a new child environment, and the
  user's `.Random.seed` is restored afterwards.
* Examples that evaluate code use small inputs and `times = 10` so that
  they finish in well under a second.
