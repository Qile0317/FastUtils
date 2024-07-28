expect_lint_free <- # nolint: object_name_linter.
    if (requireNamespace("lintr", quietly = TRUE)) {
        lintr::expect_lint_free
    } else {
        function(...) skip("lintr is not installed")
    }
