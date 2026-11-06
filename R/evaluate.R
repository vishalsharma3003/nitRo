# Verification and benchmarking ----------------------------------------------
#
# These are the only functions in the package that evaluate user code. Each
# evaluation happens in a fresh child environment of `envir`, so variables
# assigned by the code do not leak into the caller's environment.

.with_preserved_seed <- function(expr) {
  genv <- globalenv()
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = genv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = genv)
    } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(".Random.seed", envir = genv)
    }
  })
  expr
}

.run_once <- function(exprs, envir) {
  env <- new.env(parent = envir)
  set.seed(1L)
  value <- tryCatch(
    {
      result <- NULL
      for (e in exprs) result <- eval(e, envir = env)
      result
    },
    error = function(e) e
  )
  list(value = value, env = env, error = inherits(value, "error"))
}

#' Check that the optimised code gives the same result as the original
#'
#' Both versions are run once with the same random seed. They are considered
#' equivalent when the value of the last expression and every variable they
#' create are `identical()`.
#'
#' @return A list with `status` (one of `"identical"`, `"different"`,
#'   `"original_error"`, `"optimised_error"`) and `message`.
#' @noRd
.verify <- function(original, optimised, envir) {
  .with_preserved_seed({
    a <- .run_once(original, envir)
    b <- .run_once(optimised, envir)
  })

  if (a$error) {
    return(list(
      status  = "original_error",
      message = paste("The original code failed:", conditionMessage(a$value))
    ))
  }
  if (b$error) {
    return(list(
      status  = "optimised_error",
      message = paste("The optimised code failed:", conditionMessage(b$value))
    ))
  }

  vars_a <- as.list(a$env, all.names = TRUE, sorted = TRUE)
  vars_b <- as.list(b$env, all.names = TRUE, sorted = TRUE)

  if (identical(a$value, b$value) && identical(vars_a, vars_b)) {
    list(
      status  = "identical",
      message = "The optimised code produced identical results."
    )
  } else {
    list(
      status  = "different",
      message = paste(
        "The optimised code produced different results.",
        "Review the changes before using them."
      )
    )
  }
}

#' Time the original and optimised code with microbenchmark
#'
#' @return A `microbenchmark` object.
#' @noRd
.benchmark <- function(original, optimised, envir, times) {
  env_a <- new.env(parent = envir)
  env_b <- new.env(parent = envir)
  run   <- function(exprs, env) {
    for (e in exprs) eval(e, envir = env)
    invisible(NULL)
  }
  .with_preserved_seed(
    microbenchmark::microbenchmark(
      original  = run(original, env_a),
      optimised = run(optimised, env_b),
      times     = times
    )
  )
}

.speedup <- function(bench) {
  med <- tapply(bench$time, bench$expr, stats::median)
  list(
    median_original_ns  = unname(med[["original"]]),
    median_optimised_ns = unname(med[["optimised"]]),
    speedup             = unname(med[["original"]] / med[["optimised"]])
  )
}
