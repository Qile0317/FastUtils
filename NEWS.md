# FastUtils (development version)

## Additions

- Add `removeVdiffrNewSnapShots()` with its alias `rmns()` to remove all new snapshots created by `vdiffr` in the current working directory.
- the pkgdown site is now tracked with google analytics
- Add `setNames()` as an improved version of `stats::setNames()`
- the package is now internally linted with `lintr`

## Changes

- the following arguments were renamed from using `snake_case` to `camelCase`:
    - `init_vals` in `createHash()` is now `initVals`
    - `zero_indexed` in `enumerateit()` is now `zeroIndexed`
- many functions now have assertions about its inputs

# FastUtils 0.1.1

Initial CRAN release. Many functions err more on the "readability" side rather than the "fast" side, but performance improvements are to be added in every upcoming release as the rate of increase in exported functions per release slow down.
