#' List the rules nitRo checks
#'
#' @param category Optional character vector of categories to keep. See
#'   [nitro()] for the possible values.
#' @param severity Optional character vector of severity levels to keep.
#' @return A data frame with one row per rule and columns `id`, `category`,
#'   `severity`, `auto_fix` (whether nitRo rewrites matches automatically),
#'   `message` and `reference`.
#' @export
#' @examples
#' rules <- list_rules()
#' nrow(rules)
#' table(rules$category)
#'
#' # Rules that rewrite code automatically
#' list_rules()[list_rules()$auto_fix, c("id", "message")]
#'
#' list_rules(category = "dplyr")
list_rules <- function(category = NULL, severity = NULL) {
  if (!is.null(category)) .check_in(category, .nitro_categories, "category")
  if (!is.null(severity)) .check_in(severity, .nitro_severities, "severity")
  rules <- .get_rules(category = category, severity = severity)
  data.frame(
    id        = vapply(rules, `[[`, character(1), "id"),
    category  = vapply(rules, `[[`, character(1), "category"),
    severity  = vapply(rules, `[[`, character(1), "severity"),
    auto_fix  = vapply(rules, `[[`, logical(1), "auto_fix"),
    message   = vapply(rules, `[[`, character(1), "message"),
    reference = vapply(rules, `[[`, character(1), "reference"),
    stringsAsFactors = FALSE
  )
}
