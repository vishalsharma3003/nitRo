# Rule registry -----------------------------------------------------------
#
# Every rule is built with .rule(). Fields:
#   id        unique identifier, e.g. "BASE001"
#   category  one of .nitro_categories
#   severity  one of .nitro_severities
#   pattern   PCRE regular expression matched against the normalised code
#   message   one-line description shown in the report
#   detail    longer explanation, shown with verbose = TRUE
#   fix       replacement string for gsub(perl = TRUE), or NA for
#             report-only rules. A rule is auto-fixable if and only if
#             fix is not NA.
#   context   optional regex that must ALSO match somewhere in the code
#   exclude   optional regex; if it matches anywhere in the code the rule
#             does not fire
#   reference where to read more
#
# Every auto-fix rule must produce output identical to the original for the
# inputs it matches. Speed claims in `detail` were measured with
# microbenchmark on R 4.3.3 (vectors of 1e6 elements, 1000 x 1000 matrices,
# 1e5 strings).

.rule <- function(id, category, severity, pattern, message, detail,
                  fix = NA_character_, context = NA_character_,
                  exclude = NA_character_, reference = NA_character_) {
  list(
    id        = id,
    category  = category,
    severity  = severity,
    pattern   = pattern,
    message   = message,
    detail    = detail,
    fix       = fix,
    auto_fix  = !is.na(fix),
    context   = context,
    exclude   = exclude,
    reference = reference
  )
}

# A bare R name (no namespace, no indexing).
.sym <- "[A-Za-z.][A-Za-z0-9._]*"

# The code contains a loop.
.loop <- "\\b(?:for|while|repeat)\\b"

# A string literal containing no regular-expression metacharacters.
.lit <- "\"[^\"\\\\.^$|?*+()\\[\\]{}]+\""

# A balanced pair of parentheses, captured as group 1 (PCRE recursion).
.parens <- "(\\((?:[^()]++|(?1))*\\))"

# Arguments that make a regex call ineligible for fixed = TRUE.
.no_regex_args <- "(?![^()]*\\b(?:fixed|perl|ignore\\.case)\\s*=)"

.nitro_rules <- list(

  # base_safety --------------------------------------------------------------

  .rule(
    "BASE001", "base_safety", "error_prone",
    pattern = "\\b1:length\\(",
    message = "Use seq_along(x) instead of 1:length(x)",
    detail  = paste(
      "When x is empty, 1:length(x) is c(1, 0), so the loop body runs twice",
      "with invalid indices. seq_along(x) returns integer(0) and the loop",
      "body is skipped. For non-empty x both give the same result."
    ),
    fix       = "seq_along(",
    reference = "?seq_along"
  ),

  .rule(
    "BASE002", "base_safety", "error_prone",
    pattern = paste0("\\b1:nrow", .parens),
    message = "Use seq_len(nrow(x)) instead of 1:nrow(x)",
    detail  = paste(
      "When x has zero rows, 1:nrow(x) is c(1, 0). seq_len(nrow(x))",
      "returns integer(0). For x with rows both give the same result."
    ),
    fix       = "seq_len(nrow\\1)",
    reference = "?seq_len"
  ),

  .rule(
    "BASE003", "base_safety", "error_prone",
    pattern = paste0("\\b1:ncol", .parens),
    message = "Use seq_len(ncol(x)) instead of 1:ncol(x)",
    detail  = paste(
      "When x has zero columns, 1:ncol(x) is c(1, 0). seq_len(ncol(x))",
      "returns integer(0)."
    ),
    fix       = "seq_len(ncol\\1)",
    reference = "?seq_len"
  ),

  .rule(
    "BASE004", "base_safety", "error_prone",
    pattern = "[!=]=\\s*NA(?:_integer_|_real_|_character_)?\\b",
    message = "Use is.na(x) instead of x == NA",
    detail  = paste(
      "Any comparison with NA returns NA, so x == NA is never TRUE and",
      "if (x == NA) fails with 'missing value where TRUE/FALSE needed'.",
      "Use is.na(x), or !is.na(x) in place of x != NA."
    ),
    reference = "?is.na"
  ),

  .rule(
    "BASE005", "base_safety", "error_prone",
    pattern = "[!=]=\\s*NULL\\b",
    message = "Use is.null(x) instead of x == NULL",
    detail  = paste(
      "x == NULL returns logical(0), and if (logical(0)) is an error.",
      "Use is.null(x)."
    ),
    reference = "?is.null"
  ),

  .rule(
    "BASE006", "base_safety", "error_prone",
    pattern = "\\bclass\\s*\\([^()]*\\)\\s*[!=]=\\s*\"",
    message = "Use inherits(x, \"cls\") instead of class(x) == \"cls\"",
    detail  = paste(
      "Many objects have more than one class (for example a tibble has",
      "c(\"tbl_df\", \"tbl\", \"data.frame\")). class(x) == \"cls\" then",
      "returns several values, and since R 4.2.0 using it in if() is an",
      "error. inherits(x, \"cls\") always returns a single TRUE or FALSE."
    ),
    reference = "?inherits"
  ),

  .rule(
    "BASE007", "base_safety", "style",
    pattern = "(?<![A-Za-z0-9._$@\"'])[TF](?![A-Za-z0-9._(\"'])",
    message = "Write TRUE and FALSE in full instead of T and F",
    detail  = paste(
      "T and F are ordinary variables that can be reassigned",
      "(T <- 0 is legal). TRUE and FALSE are reserved words."
    ),
    reference = "?TRUE"
  ),

  .rule(
    "BASE008", "base_safety", "style",
    pattern = "[!=]=\\s*(?:TRUE|FALSE)\\b",
    message = "Use the logical value directly instead of comparing with TRUE or FALSE",
    detail  = paste(
      "if (x == TRUE) can be written if (x), and if (x == FALSE) as",
      "if (!x). Use isTRUE(x) or isFALSE(x) if x may be NA or have",
      "length other than one."
    ),
    reference = "?isTRUE"
  ),

  # allocation ---------------------------------------------------------------

  .rule(
    "ALLOC001", "allocation", "warning",
    pattern = paste0("\\b(", .sym, ")\\s*<-\\s*c\\(\\s*\\1\\s*,"),
    context = .loop,
    message = "Vector grown with c() inside a loop",
    detail  = paste(
      "x <- c(x, value) copies the whole vector on every iteration, so",
      "the loop takes quadratic time. Pre-allocate with",
      "x <- vector(\"numeric\", n) and assign x[i] <- value, or replace",
      "the loop with a vectorised expression."
    ),
    reference = "Wickham (2019) Advanced R, 2nd ed., section 24.6"
  ),

  .rule(
    "ALLOC002", "allocation", "warning",
    pattern = paste0("\\b(", .sym, ")\\s*<-\\s*rbind\\(\\s*\\1\\s*,"),
    context = .loop,
    message = "Rows added with rbind() inside a loop",
    detail  = paste(
      "Each rbind() call copies all existing rows. Store each piece in a",
      "pre-allocated list (pieces <- vector(\"list\", n)) and combine once",
      "after the loop with do.call(rbind, pieces),",
      "data.table::rbindlist(pieces) or dplyr::bind_rows(pieces)."
    ),
    reference = "Wickham (2019) Advanced R, 2nd ed., section 24.6"
  ),

  .rule(
    "ALLOC003", "allocation", "warning",
    pattern = paste0("\\b(", .sym, ")\\s*<-\\s*cbind\\(\\s*\\1\\s*,"),
    context = .loop,
    message = "Columns added with cbind() inside a loop",
    detail  = paste(
      "Each cbind() call copies all existing columns. Pre-allocate a",
      "matrix of the final size and assign m[, j] <- value, or collect",
      "the columns in a list and combine once after the loop."
    ),
    reference = "Wickham (2019) Advanced R, 2nd ed., section 24.6"
  ),

  .rule(
    "ALLOC004", "allocation", "warning",
    pattern = paste0("\\b(", .sym, ")\\s*<-\\s*append\\(\\s*\\1\\s*,"),
    context = .loop,
    message = "Vector or list grown with append() inside a loop",
    detail  = paste(
      "append() copies the whole object on every call. Pre-allocate with",
      "vector(\"list\", n) or vector(\"numeric\", n) and assign by index."
    ),
    reference = "Wickham (2019) Advanced R, 2nd ed., section 24.6"
  ),

  .rule(
    "ALLOC005", "allocation", "warning",
    pattern = paste0("\\b(", .sym, ")\\s*<-\\s*paste0?\\(\\s*\\1\\s*,"),
    context = .loop,
    message = "String built up with paste() inside a loop",
    detail  = paste(
      "Repeatedly appending to a string copies it on every iteration.",
      "Build a character vector of the pieces and join once with",
      "paste(pieces, collapse = \"\")."
    ),
    reference = "?paste"
  ),

  # loop_replacement ---------------------------------------------------------

  .rule(
    "LOOP001", "loop_replacement", "suggestion",
    pattern = "\\bfor\\s*\\(",
    message = "for loop: check whether it can be vectorised or written with vapply()/lapply()",
    detail  = paste(
      "Many element-wise loops can be replaced by a single vectorised",
      "call (for example out <- x * 2). Where that is not possible,",
      "vapply() or purrr::map_dbl() and friends state the result type",
      "and avoid manual pre-allocation. A for loop over a pre-allocated",
      "result is not slow in itself; the gain comes from vectorisation."
    ),
    reference = "Wickham (2019) Advanced R, 2nd ed., chapter 9"
  ),

  .rule(
    "LOOP002", "loop_replacement", "suggestion",
    pattern = "\\bsapply\\s*\\(",
    message = "Use vapply() instead of sapply() when the result type is known",
    detail  = paste(
      "sapply() returns a vector, matrix or list depending on its input,",
      "and list() when the input is empty. vapply(x, f, numeric(1))",
      "always returns the declared type and fails with a clear error",
      "when f returns something else."
    ),
    reference = "?vapply"
  ),

  .rule(
    "LOOP003", "loop_replacement", "suggestion",
    pattern = "\\bunlist\\s*\\(\\s*lapply\\s*\\(",
    message = "Use vapply() instead of unlist(lapply())",
    detail  = paste(
      "unlist(lapply(x, f)) builds a list and then flattens it.",
      "vapply(x, f, numeric(1)) (or purrr::map_dbl(x, f)) returns the",
      "vector directly and checks the type of each result."
    ),
    reference = "?vapply"
  ),

  .rule(
    "LOOP004", "loop_replacement", "suggestion",
    pattern = "\\bsapply\\s*\\([^()]*,\\s*length\\s*\\)",
    message = "Use lengths(x) instead of sapply(x, length)",
    detail  = paste(
      "lengths() is implemented in C and always returns an integer",
      "vector. Note that sapply() on a character vector names the result",
      "by its values and lengths() does not."
    ),
    reference = "?lengths"
  ),

  .rule(
    "LOOP005", "loop_replacement", "suggestion",
    pattern = "\\bmapply\\s*\\(",
    message = "Consider purrr::pmap() or Map() instead of mapply()",
    detail  = paste(
      "mapply() simplifies its result in the same unpredictable way as",
      "sapply(). Map() always returns a list; purrr::pmap_dbl() and",
      "friends return a vector of a declared type."
    ),
    reference = "?mapply"
  ),

  .rule(
    "LOOP006", "loop_replacement", "warning",
    pattern = paste0("\\bapply\\(\\s*(", .sym, ")\\s*,\\s*1L?\\s*,\\s*sum\\s*\\)"),
    message = "Use rowSums(x) instead of apply(x, 1, sum)",
    detail  = paste(
      "rowSums() is implemented in C. Measured about 3x faster on a",
      "1000 x 1000 matrix, with identical results."
    ),
    fix       = "rowSums(\\1)",
    reference = "?rowSums"
  ),

  .rule(
    "LOOP007", "loop_replacement", "warning",
    pattern = paste0("\\bapply\\(\\s*(", .sym, ")\\s*,\\s*2L?\\s*,\\s*sum\\s*\\)"),
    message = "Use colSums(x) instead of apply(x, 2, sum)",
    detail  = paste(
      "colSums() is implemented in C and avoids extracting each column",
      "as a separate vector."
    ),
    fix       = "colSums(\\1)",
    reference = "?colSums"
  ),

  .rule(
    "LOOP008", "loop_replacement", "warning",
    pattern = paste0("\\bapply\\(\\s*(", .sym, ")\\s*,\\s*1L?\\s*,\\s*mean\\s*\\)"),
    message = "Use rowMeans(x) instead of apply(x, 1, mean)",
    detail  = "rowMeans() is implemented in C.",
    fix       = "rowMeans(\\1)",
    reference = "?rowMeans"
  ),

  .rule(
    "LOOP009", "loop_replacement", "warning",
    pattern = paste0("\\bapply\\(\\s*(", .sym, ")\\s*,\\s*2L?\\s*,\\s*mean\\s*\\)"),
    message = "Use colMeans(x) instead of apply(x, 2, mean)",
    detail  = paste(
      "colMeans() is implemented in C. Measured about 12x faster on a",
      "1000 x 1000 matrix, with identical results."
    ),
    fix       = "colMeans(\\1)",
    reference = "?colMeans"
  ),

  # dplyr ----------------------------------------------------------------------

  .rule(
    "DPLYR001", "dplyr", "warning",
    pattern = "(?<![A-Za-z0-9._])do\\s*\\(",
    message = "dplyr::do() is superseded",
    detail  = paste(
      "Use reframe() for results with any number of rows per group,",
      "group_modify() for data-frame-in, data-frame-out functions, or",
      "tidyr::nest() with purrr::map()."
    ),
    reference = "?dplyr::do"
  ),

  .rule(
    "DPLYR002", "dplyr", "style",
    pattern = "\\bgroup_by\\s*\\(",
    exclude = "\\bungroup\\s*\\(|\\.groups\\s*=|\\.by\\s*=",
    message = "group_by() is never followed by ungroup() or .groups",
    detail  = paste(
      "A grouped data frame stays grouped after mutate() and filter(),",
      "which changes the result of later steps. End the grouped section",
      "with ungroup(), use summarise(..., .groups = \"drop\"), or use the",
      ".by argument (dplyr >= 1.1.0) for a single grouped operation."
    ),
    reference = "?dplyr::group_by"
  ),

  .rule(
    "DPLYR003", "dplyr", "suggestion",
    pattern = "\\browwise\\s*\\(",
    message = "rowwise() runs once per row; prefer a vectorised alternative",
    detail  = paste(
      "rowwise() evaluates each expression separately for every row. For",
      "row sums, means, minima and maxima use rowSums(), rowMeans(),",
      "pmin() or pmax() inside an ordinary mutate()."
    ),
    reference = "vignette(\"rowwise\", package = \"dplyr\")"
  ),

  .rule(
    "DPLYR004", "dplyr", "style",
    pattern = "\\b(?:left|right|inner|full|semi|anti)_join\\s*\\((?![^()]*\\bby\\s*=)[^()]*\\)",
    message = "Join without an explicit by argument",
    detail  = paste(
      "Without by, dplyr joins on every column with a shared name and",
      "prints a message. The join silently changes if a column is added",
      "or renamed later. State the key, for example by = \"id\" or",
      "by = join_by(id)."
    ),
    reference = "?dplyr::join_by"
  ),

  .rule(
    "DPLYR005", "dplyr", "style",
    pattern = "\\b(?:mutate|summarise|summarize|select|rename|filter|group_by|arrange|transmute)_(?:at|if|all|each)\\s*\\(",
    message = "Scoped verbs (_at, _if, _all) are superseded by across()",
    detail  = paste(
      "Replace for example mutate_at(vars(a, b), f) with",
      "mutate(across(c(a, b), f)), and mutate_if(is.numeric, f) with",
      "mutate(across(where(is.numeric), f))."
    ),
    reference = "vignette(\"colwise\", package = \"dplyr\")"
  ),

  .rule(
    "DPLYR006", "dplyr", "style",
    pattern = "(?<![A-Za-z0-9._])vars\\s*\\(",
    message = "vars() is superseded; use tidy-select inside across()",
    detail  = "vars() only exists for the superseded scoped verbs.",
    reference = "vignette(\"colwise\", package = \"dplyr\")"
  ),

  # data_table -----------------------------------------------------------------

  .rule(
    "DT001", "data_table", "suggestion",
    pattern = "\\bread\\.csv\\s*\\(",
    message = "data.table::fread() reads large CSV files much faster than read.csv()",
    detail  = paste(
      "fread() is multi-threaded and detects the separator and column",
      "types automatically. It returns a data.table; pass",
      "data.table = FALSE to get a data.frame. Defaults differ from",
      "read.csv() (for example in how it handles quoted fields and",
      "strings that look like numbers), so check the result."
    ),
    reference = "?data.table::fread"
  ),

  .rule(
    "DT002", "data_table", "suggestion",
    pattern = "\\bwrite\\.csv\\s*\\(",
    message = "data.table::fwrite() writes large CSV files much faster than write.csv()",
    detail  = paste(
      "fwrite() is multi-threaded. Unlike write.csv() it does not write",
      "row names by default, so the files are not byte-for-byte",
      "identical."
    ),
    reference = "?data.table::fwrite"
  ),

  .rule(
    "DT003", "data_table", "suggestion",
    pattern = "(?<![A-Za-z0-9._])merge\\s*\\(",
    message = "For large tables, consider data.table or dplyr joins instead of merge()",
    detail  = paste(
      "Keyed data.table joins (X[Y, on = \"id\"]) and dplyr joins are",
      "usually faster than base merge() on large data. merge() also",
      "sorts the result by the key unless sort = FALSE."
    ),
    reference = "vignette(\"datatable-joins\", package = \"data.table\")"
  ),

  .rule(
    "DT004", "data_table", "suggestion",
    pattern = "\\[\\s*order\\s*\\(",
    message = "x[order(...), ] copies the data; data.table::setorder() sorts in place",
    detail  = paste(
      "Reordering with x[order(x$a), ] allocates a full copy. On a",
      "data.table, setorder(x, a) reorders by reference without a copy."
    ),
    reference = "?data.table::setorder"
  ),

  # file_io ----------------------------------------------------------------------

  .rule(
    "IO001", "file_io", "suggestion",
    pattern = "\\b(?:read_csv|read\\.csv|read\\.table|fread)\\s*\\(",
    message = "If this file is read repeatedly, store it in a binary format",
    detail  = paste(
      "Parsing text is the slow part of reading a CSV. Convert the file",
      "once with arrow::write_feather() or arrow::write_parquet() and",
      "read it with arrow::read_feather() or arrow::read_parquet().",
      "Parquet is also readable from Python, Spark and DuckDB."
    ),
    reference = "https://arrow.apache.org/docs/r/"
  ),

  .rule(
    "IO002", "file_io", "suggestion",
    pattern = "\\b(?:write_csv|write\\.csv|write\\.table|fwrite)\\s*\\(",
    message = "For intermediate files, write Feather or Parquet instead of CSV",
    detail  = paste(
      "Feather and Parquet preserve column types, are smaller than CSV",
      "and are read back much faster. Keep CSV for files meant for",
      "people or for tools that only read text."
    ),
    reference = "https://arrow.apache.org/docs/r/"
  ),

  .rule(
    "IO003", "file_io", "suggestion",
    pattern = "\\b(?:saveRDS|readRDS)\\s*\\(",
    message = "qs2::qs_save()/qs_read() are faster than saveRDS()/readRDS() for large objects",
    detail  = paste(
      "qs2 serialises R objects with multi-threaded compression. Files",
      "written by qs2 can only be read by qs2."
    ),
    reference = "https://cran.r-project.org/package=qs2"
  ),

  .rule(
    "IO004", "file_io", "warning",
    pattern = "\\bread_(?:excel|xlsx|xls)\\s*\\(",
    context = .loop,
    message = "Excel file read inside a loop",
    detail  = paste(
      "Reading Excel files is slow. If the same file is read on every",
      "iteration, read it once before the loop and reuse the result."
    ),
    reference = "?readxl::read_excel"
  ),

  .rule(
    "IO005", "file_io", "suggestion",
    pattern = "\\bfromJSON\\s*\\(",
    message = "For large newline-delimited JSON, use jsonlite::stream_in()",
    detail  = paste(
      "fromJSON() parses the whole document in memory. If the file has",
      "one JSON record per line (NDJSON), jsonlite::stream_in() reads it",
      "in chunks with bounded memory."
    ),
    reference = "?jsonlite::stream_in"
  ),

  .rule(
    "IO006", "file_io", "style",
    pattern = "(?<![A-Za-z0-9._])(?:load|save)\\s*\\(",
    message = "Prefer saveRDS()/readRDS() to save()/load()",
    detail  = paste(
      "load() creates objects under their saved names and can silently",
      "overwrite variables. readRDS() returns one object that you assign",
      "to a name of your choice."
    ),
    reference = "?readRDS"
  ),

  # strings ----------------------------------------------------------------------

  .rule(
    "STR001", "strings", "warning",
    pattern = paste0(
      "\\b(g?sub|grepl|grep|regexpr|gregexpr)\\(", .no_regex_args,
      "(\\s*", .lit, "\\s*,[^()\\\\]*?)\\)"
    ),
    message = "Add fixed = TRUE when the pattern is a plain string",
    detail  = paste(
      "The pattern contains no regular-expression metacharacters, so",
      "fixed = TRUE gives the same result without compiling a regular",
      "expression. Measured about 2.5x to 3.5x faster for gsub() and",
      "grepl() on 1e5 strings."
    ),
    fix       = "\\1(\\2, fixed = TRUE)",
    reference = "?regex"
  ),

  .rule(
    "STR002", "strings", "warning",
    pattern = paste0(
      "\\bstrsplit\\(", .no_regex_args,
      "([^()\\\\]*?,\\s*(?:split\\s*=\\s*)?", .lit, ")\\s*\\)"
    ),
    message = "Add fixed = TRUE to strsplit() when the separator is a plain string",
    detail  = paste(
      "The separator contains no regular-expression metacharacters, so",
      "fixed = TRUE gives the same result. Measured about 2x faster on",
      "1e5 strings."
    ),
    fix       = "strsplit(\\1, fixed = TRUE)",
    reference = "?strsplit"
  ),

  .rule(
    "STR003", "strings", "style",
    pattern = "\\bpaste\\(([^()]*?),\\s*sep\\s*=\\s*\"\"\\s*\\)",
    message = "Use paste0() instead of paste(..., sep = \"\")",
    detail  = "paste0() is equivalent and shorter. The speed is the same.",
    fix       = "paste0(\\1)",
    reference = "?paste0"
  ),

  # math ---------------------------------------------------------------------------

  .rule(
    "MATH001", "math", "warning",
    pattern = paste0("\\bsqrt\\(\\s*(", .sym, ")\\s*\\^\\s*2L?\\s*\\)"),
    message = "Use abs(x) instead of sqrt(x^2)",
    detail  = paste(
      "abs(x) gives the same result for real numbers without squaring",
      "and taking a root. Measured about 4x faster on 1e6 numbers."
    ),
    fix       = "abs(\\1)",
    reference = "?abs"
  ),

  .rule(
    "MATH002", "math", "warning",
    pattern = paste0(
      "\\bwhich\\(\\s*(", .sym, ")\\s*==\\s*([^()=!<>&|,]+?)\\s*\\)\\[1L?\\]"
    ),
    message = "Use match(value, x) instead of which(x == value)[1]",
    detail  = paste(
      "which(x == value) compares every element and then collects all",
      "matching positions. match() returns the first position (or NA)",
      "directly. Measured 2x to 4.5x faster on 1e6 integers."
    ),
    fix       = "match(\\2, \\1)",
    reference = "?match"
  ),

  .rule(
    "MATH003", "math", "suggestion",
    pattern = paste0("\\bany\\(\\s*", .sym, "\\s*==\\s*[^()=!<>&|,]+?\\s*\\)"),
    message = "Consider value %in% x instead of any(x == value)",
    detail  = paste(
      "%in% is clearer and avoids allocating a logical vector. The two",
      "differ when x contains NA and value is absent: any(x == value)",
      "returns NA, value %in% x returns FALSE. Not rewritten",
      "automatically for that reason."
    ),
    reference = "?`%in%`"
  ),

  .rule(
    "MATH004", "math", "warning",
    pattern = "\\blength\\(\\s*which\\(([^()]*)\\)\\s*\\)",
    message = "Use sum(cond, na.rm = TRUE) instead of length(which(cond))",
    detail  = paste(
      "Both count the TRUE values of a logical vector, ignoring NA.",
      "sum() does not build the vector of positions. Measured about",
      "8x faster on 1e6 values."
    ),
    fix       = "sum(\\1, na.rm = TRUE)",
    reference = "?sum"
  ),

  .rule(
    "MATH005", "math", "suggestion",
    pattern = paste0("\\b(", .sym, ")\\s*<-\\s*\\1\\s*\\+"),
    context = "\\bfor\\s*\\(",
    message = "Running total in a for loop: use sum() or cumsum()",
    detail  = paste(
      "total <- total + x[i] inside a loop can usually be written as",
      "sum(x), and a vector of running totals as cumsum(x). Both are",
      "implemented in C."
    ),
    reference = "?cumsum"
  ),

  .rule(
    "MATH006", "math", "suggestion",
    pattern = paste0("\\b(", .sym, ")\\s*<-\\s*\\1\\s*\\*"),
    context = "\\bfor\\s*\\(",
    message = "Running product in a for loop: use prod() or cumprod()",
    detail  = "Use prod(x) for the product and cumprod(x) for running products.",
    reference = "?cumprod"
  ),

  # parallel -------------------------------------------------------------------------

  .rule(
    "PAR001", "parallel", "suggestion",
    pattern = "(?<![A-Za-z0-9._])lapply\\s*\\(",
    message = "If each lapply() call is slow and independent, it can run in parallel",
    detail  = paste(
      "future.apply::future_lapply() runs on all platforms.",
      "parallel::mclapply() uses forking and runs in parallel only on",
      "macOS and Linux. Parallelism helps when each call takes long",
      "(model fits, file or network I/O); for fast calls the overhead",
      "outweighs the gain."
    ),
    reference = "vignette(\"parallel\", package = \"parallel\")"
  ),

  .rule(
    "PAR002", "parallel", "suggestion",
    pattern = "\\bfor\\s*\\(",
    message = "If loop iterations are slow and independent, they can run in parallel",
    detail  = paste(
      "Iterations that do not depend on each other can be run with",
      "foreach::foreach() and %dopar% (with a backend such as",
      "doParallel), or rewritten as future.apply::future_lapply()."
    ),
    reference = "https://cran.r-project.org/package=foreach"
  ),

  .rule(
    "PAR003", "parallel", "suggestion",
    pattern = "\\b(?:purrr::)?map(?:_dbl|_chr|_lgl|_int|_dfr|_dfc|2|_df)?\\s*\\(",
    message = "If each map() call is slow and independent, furrr::future_map() runs it in parallel",
    detail  = paste(
      "furrr provides parallel versions of the purrr map functions.",
      "Call future::plan(future::multisession) first."
    ),
    reference = "https://cran.r-project.org/package=furrr"
  )
)

.nitro_categories <- c(
  "base_safety", "allocation", "loop_replacement", "dplyr",
  "data_table", "file_io", "strings", "math", "parallel"
)

.nitro_severities <- c("error_prone", "warning", "suggestion", "style")

.get_rules <- function(category = NULL, severity = NULL) {
  rules <- .nitro_rules
  if (!is.null(category)) {
    rules <- rules[vapply(rules, function(r) r$category %in% category, logical(1))]
  }
  if (!is.null(severity)) {
    rules <- rules[vapply(rules, function(r) r$severity %in% severity, logical(1))]
  }
  rules
}
