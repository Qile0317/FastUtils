# FastUtils (development version)

## Additions

- the pkgdown site is now tracked with google analytics
- the package is now internally linted with `lintr`

## Changes

- the following arguments were renamed from using `snake_case` to `camelCase`:
    - `init_vals` in `createHash()` is now `initVals`
    - `zero_indexed` in `enumerateit()` is now `zeroIndexed`

# FastUtils 0.1.1

Initial CRAN release. Many functions err more on the "readability" side rather than the "fast" side, but performance improvements are to be added in every upcoming release as the rate of increase in exported functions per release slow down.
