# FastUtils 0.2.1

## Additions

- Add `removeVdiffrNewSnapShots()` with its alias `rmns()` to remove all new snapshots created by `vdiffr` in the current working directory.
- Add `setNames()` as an improved version of `stats::setNames()`
- Add `getFailStr()` that gets the full failure message from a condition object as a string
- Add `pia()` as a short alias to `prependIndefArticle()`
- Add `evalText()` to evaluate a characters as an R expression
- the package is now internally linted with `lintr`
- the pkgdown site is now tracked with google analytics

## Changes

- the following arguments were renamed from using `snake_case` to `camelCase`:
    - `init_vals` in `createHash()` is now `initVals`
    - `zero_indexed` in `enumerateit()` is now `zeroIndexed`
- The `checks` argument of `validateObject()` now allows the input to also just be a single function
- many functions now have assertions about its inputs
- corrected and improved some documentation

# FastUtils 0.1.1

Initial CRAN release. Many functions err more on the "readability" side rather than the "fast" side, but performance improvements are to be added in every upcoming release as the rate of increase in exported functions per release slow down.
