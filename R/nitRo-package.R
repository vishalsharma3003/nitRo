#' @keywords internal
#' @details
#' The main function is [nitro()], which analyses an R expression. Use
#' [nitro_file()] for a script and [list_rules()] to see every rule.
#'
#' Each rule has a severity:
#' \describe{
#'   \item{error_prone}{The pattern can give wrong results or errors in
#'     some cases, for example `1:length(x)` when `x` is empty.}
#'   \item{warning}{The pattern is measurably slower than an equivalent.}
#'   \item{suggestion}{An alternative may be faster, depending on the data
#'     or the context.}
#'   \item{style}{Readability or robustness; no effect on speed.}
#' }
#'
#' Only rules whose rewrite gives identical results are fixed
#' automatically; the others are reported with an explanation.
"_PACKAGE"
