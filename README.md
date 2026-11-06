# nitRo

<!-- badges: start -->
[![R-CMD-check](https://github.com/vishalsharma3003/nitRo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vishalsharma3003/nitRo/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

nitRo finds slow or unsafe patterns in R code and rewrites the ones it can
fix safely. You pass it code the same way you pass code to
`microbenchmark()`. It reports what it found and returns an optimised
version. If it changed anything, it can also run both versions to confirm
the results are identical and show how much faster the new one is.

## Installation

```r
# install.packages("remotes")
remotes::install_github("vishalsharma3003/nitRo", ref = "dev")
```

To also build the vignettes:

```r
remotes::install_github("vishalsharma3003/nitRo", ref = "dev", build_vignettes = TRUE)
```

`devtools::install_github()` accepts the same arguments.

## Example

```r
library(nitRo)

m <- matrix(runif(1e6), nrow = 1000)

nitro({
  row_totals <- apply(m, 1, sum)
  col_means  <- apply(m, 2, mean)
  n_big      <- length(which(m > 0.9))
}, times = 20)
```

```
-- nitRo report ------------------------------------------------------------
3 findings: 3 warnings

! LOOP006 (line 1): Use rowSums(x) instead of apply(x, 1, sum) [fixed]
! LOOP009 (line 2): Use colMeans(x) instead of apply(x, 2, mean) [fixed]
! MATH004 (line 3): Use sum(cond, na.rm = TRUE) instead of length(which(cond)) [fixed]

-- Optimised code ----------------------------------------------------------
row_totals <- rowSums(m)
col_means <- colMeans(m)
n_big <- sum(m > 0.9, na.rm = TRUE)

-- Verification ------------------------------------------------------------
v The optimised code produced identical results.

-- Benchmark (median of 20 runs each) --------------------------------------
original:  26.33 ms
optimised: 7.00 ms
ratio:     3.76x
```

The timings above come from one run and will differ on your machine.

Many findings cannot be fixed automatically. nitRo reports them with an
explanation but leaves the code unchanged:

```r
nitro({
  out <- c()
  for (i in 1:length(x)) {
    out <- c(out, x[i] * 2)
  }
}, benchmark = FALSE)
```

```
-- nitRo report ------------------------------------------------------------
4 findings: 1 error-prone, 1 warning, 2 suggestions

x BASE001 (line 2): Use seq_along(x) instead of 1:length(x) [fixed]
! ALLOC001 (line 3): Vector grown with c() inside a loop
i LOOP001 (line 2): for loop: check whether it can be vectorised or written with vapply()/lapply()
i PAR002 (line 2): If loop iterations are slow and independent, they can run in parallel

-- Optimised code ----------------------------------------------------------
out <- c()
for (i in seq_along(x)) {
    out <- c(out, x[i] * 2)
}
```

Use `print(result, verbose = TRUE)` to see an explanation and a reference
for each finding.

## Functions

| Function | Purpose |
|---|---|
| `nitro(code)` | Analyse an expression; optionally verify and benchmark the rewrite |
| `nitro_file(path)` | Analyse an `.R` file, keeping its comments and line numbers |
| `list_rules()` | Data frame of every rule |
| `get_optimised(x)` | The optimised code as a character vector |
| `get_findings(x)` | The findings as a data frame |

## Rules

nitRo has 50 rules. 13 of them rewrite code automatically.

| Category | Rules | Auto-fix | Examples |
|---|---|---|---|
| `base_safety` | 8 | 3 | `1:length(x)`, `x == NA`, `class(x) == "lm"`, `T`/`F` |
| `allocation` | 5 | 0 | `x <- c(x, v)` or `rbind()` inside a loop |
| `loop_replacement` | 9 | 4 | `apply(m, 1, sum)`, `sapply()`, `unlist(lapply())` |
| `dplyr` | 6 | 0 | `do()`, `rowwise()`, joins without `by`, `mutate_at()` |
| `data_table` | 4 | 0 | `read.csv()`, `write.csv()`, `merge()`, `x[order(...), ]` |
| `file_io` | 6 | 0 | CSV for intermediate data, `saveRDS()`, `load()` |
| `strings` | 3 | 3 | `gsub("a", "b", x)` without `fixed = TRUE` |
| `math` | 6 | 3 | `sqrt(x^2)`, `which(x == v)[1]`, `length(which(x))` |
| `parallel` | 3 | 0 | independent `lapply()`, `for` and `map()` calls |

Every rule has a severity:

- **error-prone**: can give wrong results or errors in some cases.
- **warning**: measurably slower than an equivalent.
- **suggestion**: an alternative may be faster, depending on the data.
- **style**: readability or robustness only.

A rule rewrites code automatically only if the rewrite gives identical
results for the code it matches. Each automatic rewrite has a test that
runs the original and the rewrite and compares the results with
`identical()`.

## What nitRo runs

Finding and rewriting patterns never runs your code. If `benchmark = TRUE`
(the default) and at least one fix was applied, `nitro()` runs the
original and optimised code: once each to verify the results, then `times`
times each to benchmark them. Each run happens in a new environment whose
parent is the calling environment, so the code can read your variables but
cannot overwrite them. Side effects such as writing files happen on every
run. Use `benchmark = FALSE` for code that should not be run.
`nitro_file()` never runs code.

## Limitations

- Matching is done with regular expressions on the text of the code, not
  on the parse tree. A rule can fire on text inside a string literal, and
  some patterns are only recognised in their simplest form (for example
  `apply(m, 1, sum)` is fixed but `apply(m[, -1], 1, sum)` is not).
- `nitro()` reformats the expression with `deparse()`, so comments are
  dropped. Use `nitro_file()` to keep them.
- Suggestions do not know the size of your data. Advice such as using
  `data.table::fread()` or parallel processing pays off only for large
  inputs or slow calls.

## Licence

MIT © Vishal Kumar Sharma
