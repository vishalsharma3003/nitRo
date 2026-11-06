# One positive and one negative example for every rule. `hit` must trigger
# the rule and `miss` must not. For auto-fix rules, `fixed` is the expected
# rewrite of `hit`.
cases <- list(
  BASE001  = list(hit = "for (i in 1:length(x)) y[i] <- x[i]",
                  miss = "for (i in seq_along(x)) y[i] <- x[i]",
                  fixed = "for (i in seq_along(x)) y[i] <- x[i]"),
  BASE002  = list(hit = "for (i in 1:nrow(df)) df[i, ]",
                  miss = "for (i in seq_len(nrow(df))) df[i, ]",
                  fixed = "for (i in seq_len(nrow(df))) df[i, ]"),
  BASE003  = list(hit = "for (j in 1:ncol(m[, -1])) m[, j]",
                  miss = "for (j in seq_len(ncol(m))) m[, j]",
                  fixed = "for (j in seq_len(ncol(m[, -1]))) m[, j]"),
  BASE004  = list(hit = "if (x == NA) x <- 0", miss = "if (is.na(x)) x <- 0"),
  BASE005  = list(hit = "if (x != NULL) f(x)", miss = "if (!is.null(x)) f(x)"),
  BASE006  = list(hit = "if (class(x) == \"lm\") f(x)",
                  miss = "if (inherits(x, \"lm\")) f(x)"),
  BASE007  = list(hit = "mean(x, na.rm = T)",
                  miss = "y <- c(\"T\", \"F\"); mean(x, na.rm = TRUE); df$T"),
  BASE008  = list(hit = "if (ok == TRUE) go()", miss = "if (isTRUE(ok)) go()"),
  ALLOC001 = list(hit = "for (i in 1:3) out <- c(out, i)",
                  miss = "out <- c(out, 1)"),
  ALLOC002 = list(hit = "for (f in files) df <- rbind(df, read(f))",
                  miss = "df <- rbind(a, b)"),
  ALLOC003 = list(hit = "while (k < 3) m <- cbind(m, k)",
                  miss = "m <- cbind(a, b)"),
  ALLOC004 = list(hit = "for (i in 1:3) l <- append(l, i)",
                  miss = "l <- append(a, b)"),
  ALLOC005 = list(hit = "for (w in words) s <- paste0(s, w)",
                  miss = "s <- paste(words, collapse = \"\")"),
  LOOP001  = list(hit = "for (i in x) print(i)", miss = "y <- x * 2"),
  LOOP002  = list(hit = "sapply(l, mean)", miss = "vapply(l, mean, numeric(1))"),
  LOOP003  = list(hit = "unlist(lapply(l, mean))", miss = "lapply(l, mean)"),
  LOOP004  = list(hit = "sapply(l, length)", miss = "lengths(l)"),
  LOOP005  = list(hit = "mapply(f, a, b)", miss = "Map(f, a, b)"),
  LOOP006  = list(hit = "apply(m, 1, sum)", miss = "apply(m, 1, max)",
                  fixed = "rowSums(m)"),
  LOOP007  = list(hit = "apply(m, 2L, sum)", miss = "apply(m, 2, sum, na.rm = TRUE)",
                  fixed = "colSums(m)"),
  LOOP008  = list(hit = "apply(m, 1, mean)", miss = "rowMeans(m)",
                  fixed = "rowMeans(m)"),
  LOOP009  = list(hit = "apply(m, 2, mean)", miss = "colMeans(m)",
                  fixed = "colMeans(m)"),
  DPLYR001 = list(hit = "df %>% group_by(g) %>% do(fit = lm(y ~ x, .))",
                  miss = "do.call(rbind, l)"),
  DPLYR002 = list(hit = "df %>% group_by(g) %>% mutate(z = x - mean(x))",
                  miss = "df %>% group_by(g) %>% summarise(m = mean(x), .groups = \"drop\")"),
  DPLYR003 = list(hit = "df %>% rowwise() %>% mutate(t = sum(c(a, b)))",
                  miss = "df %>% mutate(t = a + b)"),
  DPLYR004 = list(hit = "left_join(a, b)", miss = "left_join(a, b, by = \"id\")"),
  DPLYR005 = list(hit = "mutate_if(df, is.numeric, round)",
                  miss = "mutate(df, across(where(is.numeric), round))"),
  DPLYR006 = list(hit = "mutate_at(df, vars(a, b), log)", miss = "across(c(a, b), log)"),
  DT001    = list(hit = "d <- read.csv(\"a.csv\")", miss = "d <- fread(\"a.csv\")"),
  DT002    = list(hit = "write.csv(d, \"a.csv\")", miss = "fwrite(d, \"a.csv\")"),
  DT003    = list(hit = "merge(a, b, by = \"id\")", miss = "setkey(a, id)"),
  DT004    = list(hit = "df[order(df$a), ]", miss = "setorder(dt, a)"),
  IO001    = list(hit = "readr::read_csv(\"a.csv\")", miss = "arrow::read_parquet(\"a.parquet\")"),
  IO002    = list(hit = "write.table(d, \"a.txt\")", miss = "arrow::write_parquet(d, \"a.parquet\")"),
  IO003    = list(hit = "saveRDS(fit, \"fit.rds\")", miss = "qs2::qs_save(fit, \"fit.qs2\")"),
  IO004    = list(hit = "for (s in sheets) d <- readxl::read_excel(p, s)",
                  miss = "d <- readxl::read_excel(p)"),
  IO005    = list(hit = "jsonlite::fromJSON(\"a.json\")", miss = "jsonlite::toJSON(x)"),
  IO006    = list(hit = "load(\"data.RData\")", miss = "download.file(u, p)"),
  STR001   = list(hit = "gsub(\"a\", \"b\", x)", miss = "gsub(\"a.\", \"b\", x)",
                  fixed = "gsub(\"a\", \"b\", x, fixed = TRUE)"),
  STR002   = list(hit = "strsplit(x, \"_\")", miss = "strsplit(x, \"_\", fixed = TRUE)",
                  fixed = "strsplit(x, \"_\", fixed = TRUE)"),
  STR003   = list(hit = "paste(\"a\", x, sep = \"\")", miss = "paste(\"a\", x, sep = \"-\")",
                  fixed = "paste0(\"a\", x)"),
  MATH001  = list(hit = "d <- sqrt(v^2)", miss = "d <- sqrt(v^2 + w^2)",
                  fixed = "d <- abs(v)"),
  MATH002  = list(hit = "i <- which(ids == target)[1]", miss = "i <- which(ids == target)",
                  fixed = "i <- match(target, ids)"),
  MATH003  = list(hit = "any(x == 3)", miss = "3 %in% x"),
  MATH004  = list(hit = "n <- length(which(x > 0))", miss = "n <- length(x)",
                  fixed = "n <- sum(x > 0, na.rm = TRUE)"),
  MATH005  = list(hit = "for (v in x) total <- total + v", miss = "total <- total + 1"),
  MATH006  = list(hit = "for (v in x) p <- p * v", miss = "p <- p * 2"),
  PAR001   = list(hit = "lapply(files, read)", miss = "future_lapply(files, read)"),
  PAR002   = list(hit = "for (f in files) read(f)", miss = "read(files)"),
  PAR003   = list(hit = "purrr::map_dbl(l, mean)", miss = "furrr::future_map(l, mean)")
)

fires <- function(id, code) {
  id %in% .analyse(strsplit(code, "\n", fixed = TRUE)[[1]])$rule_id
}

test_that("every rule has a test case", {
  ids <- vapply(.nitro_rules, `[[`, character(1), "id")
  expect_setequal(names(cases), ids)
})

for (id in names(cases)) {
  local({
    case <- cases[[id]]
    test_that(paste(id, "fires on its positive example"), {
      expect_true(fires(id, case$hit))
    })
    test_that(paste(id, "does not fire on its negative example"), {
      expect_false(fires(id, case$miss))
    })
  })
}

test_that("auto-fix rules rewrite to the expected code", {
  for (id in names(cases)) {
    rule <- Filter(function(r) r$id == id, .nitro_rules)[[1]]
    if (!rule$auto_fix) next
    expect_false(is.null(cases[[id]]$fixed), label = paste(id, "has an expected fix"))
    out <- gsub(rule$pattern, rule$fix, cases[[id]]$hit, perl = TRUE)
    expect_identical(out, cases[[id]]$fixed, label = id)
  }
})

test_that("rule registry is well formed", {
  ids <- vapply(.nitro_rules, `[[`, character(1), "id")
  expect_false(anyDuplicated(ids) > 0)
  for (rule in .nitro_rules) {
    expect_true(rule$category %in% .nitro_categories, label = rule$id)
    expect_true(rule$severity %in% .nitro_severities, label = rule$id)
    expect_identical(rule$auto_fix, !is.na(rule$fix), label = rule$id)
    expect_true(nzchar(rule$message) && nzchar(rule$detail), label = rule$id)
    expect_silent(grepl(rule$pattern, "x", perl = TRUE))
  }
})

test_that("auto-fixes give identical results when run", {
  set.seed(42)
  env <- new.env()
  env$m <- matrix(runif(200), nrow = 20)
  env$v <- c(-2.5, 0, 3, NA)
  env$x <- c(-1, 2, NA, 4)
  env$ids <- c(5L, 3L, 9L, 3L)
  env$target <- 3L
  env$s <- c("a_b", "ca_ab", NA, "")
  pairs <- list(
    c("apply(m, 1, sum)", "rowSums(m)"),
    c("apply(m, 2, sum)", "colSums(m)"),
    c("apply(m, 1, mean)", "rowMeans(m)"),
    c("apply(m, 2, mean)", "colMeans(m)"),
    c("sqrt(v^2)", "abs(v)"),
    c("which(ids == target)[1]", "match(target, ids)"),
    c("which(ids == 7L)[1]", "match(7L, ids)"),
    c("length(which(x > 0))", "sum(x > 0, na.rm = TRUE)"),
    c("gsub(\"a\", \"_\", s)", "gsub(\"a\", \"_\", s, fixed = TRUE)"),
    c("grepl(\"a\", s)", "grepl(\"a\", s, fixed = TRUE)"),
    c("strsplit(s, \"_\")", "strsplit(s, \"_\", fixed = TRUE)"),
    c("paste(\"a\", s, sep = \"\")", "paste0(\"a\", s)")
  )
  for (p in pairs) {
    expect_identical(
      eval(parse(text = p[2]), env), eval(parse(text = p[1]), env),
      label = p[2], expected.label = p[1]
    )
  }
})
