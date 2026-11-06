test_that("list_rules() returns one row per rule", {
  rules <- list_rules()
  expect_s3_class(rules, "data.frame")
  expect_identical(nrow(rules), length(.nitro_rules))
  expect_named(rules, c("id", "category", "severity", "auto_fix", "message", "reference"))
})

test_that("list_rules() filters by category and severity", {
  expect_identical(unique(list_rules(category = "dplyr")$category), "dplyr")
  expect_identical(unique(list_rules(severity = "style")$severity), "style")
  both <- list_rules(category = "math", severity = "warning")
  expect_true(all(both$category == "math" & both$severity == "warning"))
})

test_that("list_rules() rejects unknown filter values", {
  expect_error(list_rules(category = "speed"), "Unknown category")
  expect_error(list_rules(severity = 1), "character vector")
})
