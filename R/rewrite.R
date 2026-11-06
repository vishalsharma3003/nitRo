#' Apply the automatic fixes of the rules that fired
#'
#' Rules are applied one at a time, line by line, in registry order, so each
#' change can be attributed to the rule that made it.
#'
#' @param lines Character vector of code, one element per line.
#' @param findings Data frame returned by `.analyse()`.
#' @return A list with `code` (the rewritten lines) and `changes`, a data
#'   frame with one row per changed line and rule.
#' @noRd
.rewrite <- function(lines, findings) {
  fired <- findings$rule_id[findings$auto_fix]
  rules <- Filter(function(r) r$id %in% fired, .nitro_rules)

  changes <- list()
  for (rule in rules) {
    new_lines <- gsub(rule$pattern, rule$fix, lines, perl = TRUE)
    changed   <- which(new_lines != lines)
    if (length(changed) > 0L) {
      changes[[length(changes) + 1L]] <- data.frame(
        line    = changed,
        rule_id = rule$id,
        before  = lines[changed],
        after   = new_lines[changed],
        stringsAsFactors = FALSE
      )
    }
    lines <- new_lines
  }

  changes <- if (length(changes) == 0L) {
    data.frame(
      line = integer(0), rule_id = character(0),
      before = character(0), after = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, changes)
  }

  list(code = lines, changes = changes)
}
