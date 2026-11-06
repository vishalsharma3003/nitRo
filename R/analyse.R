#' Capture an unevaluated expression as lines of code
#'
#' A `{` block is split into its top-level statements; anything else is
#' deparsed as a single statement. Comments are not kept, because R drops
#' them when it parses the expression.
#'
#' @param expr An unevaluated expression, as returned by `substitute()`.
#' @return A character vector with one element per line.
#' @noRd
.capture_code <- function(expr) {
  statements <- if (is.call(expr) && identical(expr[[1L]], as.name("{"))) {
    as.list(expr)[-1L]
  } else {
    list(expr)
  }
  if (length(statements) == 0L) {
    return(character(0))
  }
  lines <- lapply(statements, deparse, width.cutoff = 500L)
  unlist(lines, use.names = FALSE)
}

.rule_fires <- function(rule, code) {
  grepl(rule$pattern, code, perl = TRUE) &&
    (is.na(rule$context) || grepl(rule$context, code, perl = TRUE)) &&
    (is.na(rule$exclude) || !grepl(rule$exclude, code, perl = TRUE))
}

#' Match code against the rule registry
#'
#' @param lines Character vector of code, one element per line.
#' @param categories,severities Optional filters, see [nitro()].
#' @return A data frame of findings, one row per rule that fired, ordered by
#'   severity and then by rule id.
#' @noRd
.analyse <- function(lines, categories = NULL, severities = NULL) {
  code  <- paste(lines, collapse = "\n")
  rules <- .get_rules(category = categories, severity = severities)
  rules <- rules[vapply(rules, .rule_fires, logical(1), code = code)]

  if (length(rules) == 0L) {
    return(.empty_findings())
  }

  line_of <- function(rule) {
    hits <- which(grepl(rule$pattern, lines, perl = TRUE))
    if (length(hits) == 0L) NA_character_ else paste(hits, collapse = ", ")
  }

  out <- data.frame(
    rule_id   = vapply(rules, `[[`, character(1), "id"),
    category  = vapply(rules, `[[`, character(1), "category"),
    severity  = vapply(rules, `[[`, character(1), "severity"),
    auto_fix  = vapply(rules, `[[`, logical(1), "auto_fix"),
    line      = vapply(rules, line_of, character(1)),
    message   = vapply(rules, `[[`, character(1), "message"),
    detail    = vapply(rules, `[[`, character(1), "detail"),
    reference = vapply(rules, `[[`, character(1), "reference"),
    stringsAsFactors = FALSE
  )

  rank <- match(out$severity, .nitro_severities)
  out  <- out[order(rank, out$rule_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.empty_findings <- function() {
  data.frame(
    rule_id   = character(0),
    category  = character(0),
    severity  = character(0),
    auto_fix  = logical(0),
    line      = character(0),
    message   = character(0),
    detail    = character(0),
    reference = character(0),
    stringsAsFactors = FALSE
  )
}
