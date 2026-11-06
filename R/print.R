#' Print a nitro report
#'
#' Prints the findings, the optimised code with the lines that changed, the
#' verification result and the benchmark medians.
#'
#' @param x An object returned by [nitro()] or [nitro_file()].
#' @param verbose Logical. If `TRUE`, also print the explanation and a
#'   reference for each finding. Default `FALSE`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
#' @examples
#' res <- nitro(for (i in 1:length(x)) y[i] <- x[i], benchmark = FALSE)
#' print(res, verbose = TRUE)
print.nitro <- function(x, verbose = FALSE, ...) {
  f <- x$findings
  cli::cat_rule(left = "nitRo report")

  if (nrow(f) == 0L) {
    cli::cat_line(cli::col_green(cli::symbol$tick), " No issues found.")
    return(invisible(x))
  }

  counts <- table(factor(f$severity, levels = .nitro_severities))
  counts <- counts[counts > 0L]
  cli::cat_line(
    nrow(f), if (nrow(f) == 1L) " finding: " else " findings: ",
    paste(counts, .severity_label(names(counts), counts), collapse = ", ")
  )
  cli::cat_line()

  width <- max(cli::console_width() - 4L, 40L)
  for (i in seq_len(nrow(f))) {
    where <- if (is.na(f$line[i])) "" else paste0(" (line ", f$line[i], ")")
    fixed <- if (f$auto_fix[i] && f$rule_id[i] %in% x$changes$rule_id) {
      cli::col_green(" [fixed]")
    } else {
      ""
    }
    cli::cat_line(
      .severity_symbol(f$severity[i]), " ", cli::style_bold(f$rule_id[i]),
      where, ": ", f$message[i], fixed
    )
    if (isTRUE(verbose)) {
      cli::cat_line(strwrap(f$detail[i], width = width, indent = 4L, exdent = 4L))
      cli::cat_line(cli::col_grey(paste0("    See: ", f$reference[i])))
    }
  }

  if (nrow(x$changes) > 0L) {
    cli::cat_line()
    cli::cat_rule(left = "Optimised code")
    cli::cat_line(x$optimised)
  }

  if (!is.null(x$verification)) {
    ok <- identical(x$verification$status, "identical")
    cli::cat_line()
    cli::cat_rule(left = "Verification")
    cli::cat_line(
      if (ok) cli::col_green(cli::symbol$tick) else cli::col_red(cli::symbol$cross),
      " ", x$verification$message
    )
  }

  if (!is.null(x$speedup)) {
    cli::cat_line()
    cli::cat_rule(left = paste0(
      "Benchmark (median of ", nrow(x$benchmark) / 2, " runs each)"
    ))
    cli::cat_line("original:  ", .format_ns(x$speedup$median_original_ns))
    cli::cat_line("optimised: ", .format_ns(x$speedup$median_optimised_ns))
    cli::cat_line("ratio:     ", format(round(x$speedup$speedup, 2), nsmall = 2), "x")
  }

  invisible(x)
}

#' Summarise a nitro analysis
#'
#' @param object An object returned by [nitro()] or [nitro_file()].
#' @param ... Ignored.
#' @return A named integer vector (returned invisibly) with the number of
#'   findings per severity level, the number of findings with an automatic
#'   fix and the number of changed lines. It is also printed.
#' @export
#' @examples
#' res <- nitro({
#'   n <- length(which(flags))
#'   s <- sapply(items, length)
#' }, benchmark = FALSE)
#' summary(res)
summary.nitro <- function(object, ...) {
  f   <- object$findings
  out <- c(
    as.vector(table(factor(f$severity, levels = .nitro_severities))),
    sum(f$auto_fix),
    length(unique(object$changes$line))
  )
  names(out) <- c(.nitro_severities, "auto_fixable", "lines_changed")
  print(out)
  if (!is.null(object$speedup)) {
    cat("speedup (median ratio):", format(round(object$speedup$speedup, 2)), "\n")
  }
  invisible(out)
}

.severity_label <- function(severity, n) {
  singular <- c(error_prone = "error-prone", warning = "warning",
                suggestion = "suggestion", style = "style")
  plural   <- c(error_prone = "error-prone", warning = "warnings",
                suggestion = "suggestions", style = "style")
  unname(ifelse(n == 1L, singular[severity], plural[severity]))
}

.severity_symbol <- function(severity) {
  switch(severity,
    error_prone = cli::col_red(cli::symbol$cross),
    warning     = cli::col_yellow(cli::symbol$warning),
    suggestion  = cli::col_cyan(cli::symbol$info),
    style       = cli::col_blue(cli::symbol$bullet)
  )
}

.format_ns <- function(ns) {
  if (ns >= 1e9) {
    sprintf("%.2f s", ns / 1e9)
  } else if (ns >= 1e6) {
    sprintf("%.2f ms", ns / 1e6)
  } else if (ns >= 1e3) {
    sprintf("%.1f us", ns / 1e3)
  } else {
    sprintf("%.0f ns", ns)
  }
}
