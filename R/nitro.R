#' Find and fix slow or unsafe patterns in R code
#'
#' `nitro()` takes R code the same way [microbenchmark::microbenchmark()]
#' does: write the code directly as the first argument, wrapped in `{ }`
#' if it spans several lines. The code is matched against the rule registry
#' (see [list_rules()]) without being run. Rules with an automatic fix are
#' applied to produce an optimised version of the code. If any fix was
#' applied and `benchmark = TRUE`, both versions are then run to check that
#' they give identical results and to time them.
#'
#' @section How the code is read:
#' The expression is captured with [substitute()] and turned back into text
#' with [deparse()]. This means comments are dropped and the code is
#' re-formatted in R's standard style. Line numbers in the report refer to
#' this normalised code, which is stored in the `original` element of the
#' result. To analyse a script with its comments and original line numbers,
#' use [nitro_file()].
#'
#' @section What is run, and where:
#' Matching and rewriting never run the code. When `benchmark = TRUE` and at
#' least one fix was applied, the original and optimised code are each run
#' once to verify the result and then `times` more times to benchmark them.
#' Every run happens in a new environment whose parent is `envir`, so the
#' code can read your variables but assignments do not change them. Code
#' with other side effects (writing files, printing, network access) has
#' those effects every time it runs. The random seed is set to the same
#' value before each verification run and your own `.Random.seed` is
#' restored afterwards.
#'
#' The two versions count as equivalent when the value of the last
#' expression and every variable created by the code are [identical()].
#' If the optimised code fails or differs, it is not benchmarked and the
#' report says so.
#'
#' @param code R code to analyse. Pass it unquoted, as you would write it in
#'   a script; use `{ }` for several lines.
#' @param categories Character vector of rule categories to check, or `NULL`
#'   (default) for all. One or more of `"base_safety"`, `"allocation"`,
#'   `"loop_replacement"`, `"dplyr"`, `"data_table"`, `"file_io"`,
#'   `"strings"`, `"math"` and `"parallel"`.
#' @param severities Character vector of severity levels to check, or
#'   `NULL` (default) for all. One or more of `"error_prone"`, `"warning"`,
#'   `"suggestion"` and `"style"`.
#' @param benchmark Logical. If `TRUE` (default) and a fix was applied, run
#'   both versions to verify and time them. Use `FALSE` when the code cannot
#'   or should not be run, for example when it refers to variables that do
#'   not exist.
#' @param times Number of times each version is run in the benchmark.
#'   Passed to [microbenchmark::microbenchmark()].
#' @param envir Environment the code is run in when `benchmark = TRUE`.
#'   Defaults to the environment `nitro()` is called from.
#'
#' @return An object of class `"nitro"`: a list with elements
#' \describe{
#'   \item{original}{Character vector: the normalised input code.}
#'   \item{optimised}{Character vector: the code after automatic fixes.
#'     Identical to `original` if no fix applied.}
#'   \item{findings}{Data frame with one row per rule that matched, with
#'     columns `rule_id`, `category`, `severity`, `auto_fix`, `line`,
#'     `message`, `detail` and `reference`.}
#'   \item{changes}{Data frame with one row per line changed by a fix, with
#'     columns `line`, `rule_id`, `before` and `after`.}
#'   \item{verification}{`NULL` if the code was not run, otherwise a list
#'     with `status` (`"identical"`, `"different"`, `"original_error"` or
#'     `"optimised_error"`) and `message`.}
#'   \item{benchmark}{`NULL`, or the `microbenchmark` object.}
#'   \item{speedup}{`NULL`, or a list with `median_original_ns`,
#'     `median_optimised_ns` and `speedup` (the ratio of the two medians).}
#' }
#' The object prints as a report; see [print.nitro()].
#'
#' @seealso [nitro_file()] to analyse a script, [list_rules()] for the rule
#'   registry, [get_optimised()] and [get_findings()] to extract results.
#' @export
#'
#' @examples
#' # Analysis only
#' nitro({
#'   total <- 0
#'   for (i in 1:length(values)) {
#'     total <- total + values[i]
#'   }
#' }, benchmark = FALSE)
#'
#' # Analysis, rewrite, verification and benchmark
#' m <- matrix(runif(1e4), nrow = 100)
#' res <- nitro({
#'   row_totals <- apply(m, 1, sum)
#'   col_means  <- apply(m, 2, mean)
#' }, times = 10)
#' res
#' get_optimised(res)
#'
#' # Only rules that point at likely bugs
#' nitro(if (x == NA) x <- 0, severities = "error_prone", benchmark = FALSE)
nitro <- function(code,
                  categories = NULL,
                  severities = NULL,
                  benchmark  = TRUE,
                  times      = 100L,
                  envir      = parent.frame()) {
  .check_args(categories, severities, benchmark, times)
  lines <- .capture_code(substitute(code))
  .nitro_run(
    lines, categories, severities,
    benchmark = benchmark, times = as.integer(times), envir = envir,
    source = "expression"
  )
}

#' Find and fix slow or unsafe patterns in an R script
#'
#' Like [nitro()], but reads the code from a file. The file is analysed as
#' written, so comments are kept and line numbers match the file. The code
#' is never run. The file is not modified; write the result yourself with
#' `writeLines(get_optimised(res), path)` if you want to keep it.
#'
#' @param path Path to an `.R` file.
#' @inheritParams nitro
#' @return An object of class `"nitro"`, as described in [nitro()], with
#'   `verification`, `benchmark` and `speedup` set to `NULL`.
#' @export
#'
#' @examples
#' path <- tempfile(fileext = ".R")
#' writeLines(c(
#'   "# summarise each row",
#'   "row_totals <- apply(m, 1, sum)",
#'   "if (class(df) == \"data.frame\") message(\"ok\")"
#' ), path)
#' res <- nitro_file(path)
#' res
#' unlink(path)
nitro_file <- function(path, categories = NULL, severities = NULL) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("`path` must be the path of an existing file.", call. = FALSE)
  }
  .check_args(categories, severities, benchmark = FALSE, times = 1L)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  tryCatch(
    parse(text = lines, keep.source = FALSE),
    error = function(e) {
      stop("`path` does not contain valid R code: ", conditionMessage(e),
           call. = FALSE)
    }
  )
  .nitro_run(
    lines, categories, severities,
    benchmark = FALSE, times = 1L, envir = globalenv(),
    source = path
  )
}

.nitro_run <- function(lines, categories, severities, benchmark, times,
                       envir, source) {
  findings  <- .analyse(lines, categories, severities)
  rewritten <- .rewrite(lines, findings)
  changed   <- !identical(lines, rewritten$code)

  verification <- NULL
  bench        <- NULL
  speedup      <- NULL

  if (benchmark && changed) {
    original  <- .parse_lines(lines)
    optimised <- .parse_lines(rewritten$code)
    if (is.null(optimised)) {
      verification <- list(
        status  = "optimised_error",
        message = "The optimised code could not be parsed."
      )
    } else {
      verification <- .verify(original, optimised, envir)
      if (identical(verification$status, "identical")) {
        bench   <- .benchmark(original, optimised, envir, times)
        speedup <- .speedup(bench)
      }
    }
  }

  structure(
    list(
      original     = lines,
      optimised    = rewritten$code,
      findings     = findings,
      changes      = rewritten$changes,
      verification = verification,
      benchmark    = bench,
      speedup      = speedup,
      source       = source
    ),
    class = "nitro"
  )
}

.parse_lines <- function(lines) {
  tryCatch(
    as.list(parse(text = lines, keep.source = FALSE)),
    error = function(e) NULL
  )
}

#' Extract results from a nitro analysis
#'
#' @param x An object returned by [nitro()] or [nitro_file()].
#' @return `get_optimised()` returns the optimised code as a character
#'   vector, one element per line. `get_findings()` returns the data frame
#'   of findings described in [nitro()].
#' @export
#' @examples
#' res <- nitro(x <- sqrt(v^2), benchmark = FALSE)
#' get_optimised(res)
#' get_findings(res)[, c("rule_id", "severity", "message")]
get_optimised <- function(x) {
  .check_nitro(x)
  x$optimised
}

#' @rdname get_optimised
#' @export
get_findings <- function(x) {
  .check_nitro(x)
  x$findings
}
