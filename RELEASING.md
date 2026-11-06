# Releasing nitRo

This file is excluded from the package build (see `.Rbuildignore`).

## 1. Test the development branch

```r
remotes::install_github("vishalsharma3003/nitRo", ref = "dev", build_vignettes = TRUE)
library(nitRo)
vignette("nitRo")
```

Every push runs the `R-CMD-check` workflow on macOS, Windows and Ubuntu
(R release, devel and oldrel-1). Results are under the **Actions** tab of
the repository.

## 2. Merge into `main`

Open a pull request from `dev` to `main` and merge it once the checks pass.
After the merge, `remotes::install_github("vishalsharma3003/nitRo")` installs
without `ref`.

The `pkgdown` workflow then builds the documentation site and pushes it to
the `gh-pages` branch. To publish it, open **Settings → Pages**, set
**Source** to *Deploy from a branch*, and choose `gh-pages` / `(root)`. The
site appears at <https://vishalsharma3003.github.io/nitRo/>.

## 3. Check before submitting to CRAN

Run these from the package directory with the `devtools` package installed.

```r
devtools::document()                         # regenerate man/ and NAMESPACE
devtools::test()                             # all tests must pass
devtools::check(args = "--as-cran")          # 0 errors, 0 warnings
spelling::spell_check_package()              # add real words to inst/WORDLIST
devtools::check_win_devel()                  # result is emailed to the maintainer
devtools::check_mac_release()                # result is emailed to the maintainer
```

On a first submission, `--as-cran` reports one NOTE, "New submission". Any
other NOTE should be fixed or explained in `cran-comments.md`.

Copy the summary line of each result into `cran-comments.md`.

## 4. Submit

```r
devtools::submit_cran()
```

This builds the package, uploads it, and asks you to confirm. CRAN then
emails the maintainer address in `DESCRIPTION`
(vishalsharma3003@gmail.com), and you must click the link in that email to
complete the submission. Reviewers usually reply within a few working days.
If they ask for changes, make them, increase the version (for example to
0.1.1), describe the changes in `cran-comments.md` and submit again.

## 5. After acceptance

```sh
git tag v0.1.0
git push origin v0.1.0
```

Then create a GitHub release from the tag, and add the CRAN install line
and CRAN badge to `README.md`:

```md
[![CRAN status](https://www.r-pkg.org/badges/version/nitRo)](https://CRAN.R-project.org/package=nitRo)
```

```r
install.packages("nitRo")
```
