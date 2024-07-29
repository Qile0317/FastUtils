test_that("validateObject works as expected", {

    # Define some checks
    checkNotNull <- function(x) if (is.null(x)) "Object is NULL"
    checkIsNumeric <- function(x) if (!is.numeric(x)) "Object is not numeric"
    checkIsPositive <- function(x) if (x <= 0) "Object is not positive"

    # Test with an object that passes all checks
    obj <- 42
    expect_equal(
        validateObject(
            obj,
            list(checkNotNull, checkIsNumeric, checkIsPositive)
        ),
        obj
    )

    # Test with an object that fails the checkNotNull check
    obj <- NULL
    expect_warning(
        result <- validateObject(
            obj,
            list(checkNotNull, checkIsNumeric, checkIsPositive)
        ),
        "Object is NULL"
    )
    expect_null(result)

    # Test with an object that fails the checkIsNumeric check
    obj <- "not a number"
    expect_warning(
        result <- validateObject(
            obj, list(checkNotNull, checkIsNumeric, checkIsPositive)
        ),
        "Object is not numeric"
    )
    expect_null(result)

    # Test with an object that fails the checkIsPositive check
    obj <- -10
    expect_warning(
        result <- validateObject(
            obj,
            list(checkNotNull, checkIsNumeric, checkIsPositive)
        ),
        "Object is not positive"
    )
    expect_null(result)

    # Test with a custom error handler that stops execution
    obj <- NULL
    expect_error(
        validateObject(
            obj, list(checkNotNull, checkIsNumeric), errorHandler = stop
        ),
        "Object is NULL"
    )

    # Test with a custom default return value
    obj <- "not a number"
    expect_warning(
        result <- validateObject(
            obj, list(checkNotNull, checkIsNumeric), defaultReturn = "default"
        ),
        "Object is not numeric"
    )
    expect_equal(result, "default")
})
