.check_in <- function(x, allowed, arg) {
  if (!is.character(x)) {
    stop(sprintf("`%s` must be a character vector.", arg), call. = FALSE)
  }
  bad <- setdiff(x, allowed)
  if (length(bad) > 0L) {
    stop(
      sprintf(
        "Unknown %s: %s. Allowed values: %s.",
        arg,
        paste0("\"", bad, "\"", collapse = ", "),
        paste0("\"", allowed, "\"", collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.check_args <- function(categories, severities, benchmark, times) {
  if (!is.null(categories)) .check_in(categories, .nitro_categories, "categories")
  if (!is.null(severities)) .check_in(severities, .nitro_severities, "severities")
  if (!is.logical(benchmark) || length(benchmark) != 1L || is.na(benchmark)) {
    stop("`benchmark` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(times) || length(times) != 1L || is.na(times) ||
      times < 1 || times != round(times)) {
    stop("`times` must be a single positive whole number.", call. = FALSE)
  }
  invisible(TRUE)
}

.check_nitro <- function(x) {
  if (!inherits(x, "nitro")) {
    stop("`x` must be an object returned by nitro() or nitro_file().",
         call. = FALSE)
  }
  invisible(TRUE)
}
