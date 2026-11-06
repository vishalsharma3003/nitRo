test_that("nitro() returns a nitro object with the documented elements", {
  res <- nitro(y <- 1, benchmark = FALSE)
  expect_s3_class(res, "nitro")
  expect_named(
    res,
    c("original", "optimised", "findings", "changes", "verification",
      "benchmark", "speedup", "source")
  )
})

test_that("nitro() captures a braced block as separate lines", {
  res <- nitro({
    a <- 1
    b <- 2
  }, benchmark = FALSE)
  expect_identical(res$original, c("a <- 1", "b <- 2"))
})

test_that("clean code produces no findings and no changes", {
  res <- nitro(y <- seq_along(x) * 2, benchmark = FALSE)
  expect_identical(nrow(res$findings), 0L)
  expect_identical(res$optimised, res$original)
})

test_that("findings report the line they occur on", {
  res <- nitro({
    a <- 1
    b <- sqrt(v^2)
  }, benchmark = FALSE)
  expect_identical(res$findings$line[res$findings$rule_id == "MATH001"], "2")
})

test_that("auto-fixes are applied and recorded per rule", {
  res <- nitro({
    r <- apply(m, 1, sum)
    k <- length(which(x > 0))
  }, benchmark = FALSE)
  expect_identical(res$optimised, c("r <- rowSums(m)", "k <- sum(x > 0, na.rm = TRUE)"))
  expect_identical(res$changes$rule_id, c("LOOP006", "MATH004"))
  expect_identical(res$changes$line, c(1L, 2L))
})

test_that("two fixes on the same line are both recorded", {
  res <- nitro(z <- sqrt(v^2) + length(which(f)), benchmark = FALSE)
  expect_identical(res$optimised, "z <- abs(v) + sum(f, na.rm = TRUE)")
  expect_setequal(res$changes$rule_id, c("MATH001", "MATH004"))
})

test_that("report-only rules do not change the code", {
  res <- nitro(if (x == NA) x <- 0, benchmark = FALSE)
  expect_true("BASE004" %in% res$findings$rule_id)
  expect_identical(res$optimised, res$original)
})

test_that("findings are ordered by severity", {
  res <- nitro({
    for (i in 1:length(x)) out <- c(out, x[i])
    y <- sapply(l, mean)
  }, benchmark = FALSE)
  rank <- match(res$findings$severity, .nitro_severities)
  expect_false(is.unsorted(rank))
})

test_that("category and severity filters are applied", {
  code_res <- nitro({
    d <- read.csv("a.csv")
    for (i in 1:length(x)) print(i)
  }, categories = "base_safety", benchmark = FALSE)
  expect_identical(unique(code_res$findings$category), "base_safety")

  sev_res <- nitro({
    d <- read.csv("a.csv")
    for (i in 1:length(x)) print(i)
  }, severities = "suggestion", benchmark = FALSE)
  expect_identical(unique(sev_res$findings$severity), "suggestion")
})

test_that("invalid arguments give informative errors", {
  expect_error(nitro(x, categories = "speed"), "Unknown categories")
  expect_error(nitro(x, severities = "critical"), "Unknown severities")
  expect_error(nitro(x, benchmark = NA), "TRUE or FALSE")
  expect_error(nitro(x, times = 0), "positive whole number")
  expect_error(nitro(x, times = 2.5), "positive whole number")
})

test_that("benchmark = FALSE never runs the code", {
  res <- nitro(stop("must not run"), benchmark = FALSE)
  expect_null(res$verification)
  expect_null(res$benchmark)
})

test_that("code is not run when no fix applies", {
  res <- nitro(stop("must not run"))
  expect_null(res$verification)
})

test_that("verification and benchmark run when a fix applies", {
  m <- matrix(runif(100), nrow = 10)
  res <- nitro({
    r <- apply(m, 1, sum)
  }, times = 5L)
  expect_identical(res$verification$status, "identical")
  expect_s3_class(res$benchmark, "microbenchmark")
  expect_identical(nrow(res$benchmark), 10L)
  expect_true(is.numeric(res$speedup$speedup) && res$speedup$speedup > 0)
})

test_that("running the code does not change the caller's variables", {
  m <- matrix(1:4, 2)
  r <- "unchanged"
  nitro(r <- apply(m, 1, sum), times = 2L)
  expect_identical(r, "unchanged")
  expect_false(exists("r", envir = globalenv(), inherits = FALSE))
})

test_that("the caller's random seed is restored", {
  set.seed(99)
  before <- .Random.seed
  v <- runif(5)
  set.seed(99)
  nitro(d <- sqrt(v^2) + runif(1), times = 2L)
  expect_identical(.Random.seed, before)
})

test_that("a failing original is reported and not benchmarked", {
  res <- nitro(d <- sqrt(no_such_variable^2), times = 2L)
  expect_identical(res$verification$status, "original_error")
  expect_null(res$benchmark)
})

test_that("nitro_file() analyses a script with its original line numbers", {
  path <- tempfile(fileext = ".R")
  on.exit(unlink(path))
  writeLines(c("# comment", "", "r <- apply(m, 1, sum)"), path)
  res <- nitro_file(path)
  expect_identical(res$findings$line[res$findings$rule_id == "LOOP006"], "3")
  expect_identical(res$optimised, c("# comment", "", "r <- rowSums(m)"))
  expect_null(res$verification)
})

test_that("nitro_file() rejects missing files and invalid R code", {
  expect_error(nitro_file(tempfile()), "existing file")
  path <- tempfile(fileext = ".R")
  on.exit(unlink(path))
  writeLines("x <- (", path)
  expect_error(nitro_file(path), "valid R code")
})

test_that("get_optimised() and get_findings() return the stored results", {
  res <- nitro(d <- sqrt(v^2), benchmark = FALSE)
  expect_identical(get_optimised(res), "d <- abs(v)")
  expect_identical(get_findings(res), res$findings)
  expect_error(get_optimised(list()), "returned by nitro")
})

test_that("print and summary methods run", {
  res <- nitro({
    for (i in 1:length(x)) y[i] <- sqrt(x[i]^2)
  }, benchmark = FALSE)
  expect_output(print(res), "BASE001")
  expect_output(print(res, verbose = TRUE), "See:")
  expect_output(print(nitro(y <- 1, benchmark = FALSE)), "No issues")
  out <- expect_output(summary(res), "error_prone")
  expect_identical(names(out), c(.nitro_severities, "auto_fixable", "lines_changed"))
})

test_that("printing a benchmarked result shows verification and timings", {
  m <- matrix(runif(100), nrow = 10)
  res <- nitro(r <- apply(m, 2, mean), times = 3L)
  expect_output(print(res), "identical results")
  expect_output(print(res), "ratio")
})
