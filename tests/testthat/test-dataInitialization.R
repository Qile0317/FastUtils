test_that("Initializing a Vector works", {
    expect_equal(initV("numeric", 5), numeric(5))
    expect_equal(initV(numeric, 5, 1), rep(1, 5))
    expect_equal(initV("character", 3), character(3))
    expect_equal(initV("numeric", 2, 3), c(3, 3))
})

test_that("initList works", {
    expect_identical(initList(), list())
    expect_identical(initList(2), list(NULL, NULL))
    expect_identical(initList(c("a", "b")), list(a = NULL, b = NULL))
    expect_identical(initList(2, 1), list(1, 1))
    expect_identical(
        initList(letters, 1),
        structure(as.list(rep(1, length(letters))), names = letters)
    )
})

test_that("initDataFrameWithColnames works as expected", {

    # Test with a single column name
    df <- initDataFrameWithColnames(c("Name"))
    expect_true(is.data.frame(df))
    expect_equal(colnames(df), "Name")
    expect_equal(nrow(df), 0)

    # Test with multiple column names
    df <- initDataFrameWithColnames(c("Name", "Age", "Gender"))
    expect_true(is.data.frame(df))
    expect_equal(colnames(df), c("Name", "Age", "Gender"))
    expect_equal(nrow(df), 0)

    # Test with no column names
    df <- initDataFrameWithColnames(character(0))
    expect_true(is.data.frame(df))
    expect_equal(colnames(df), character(0))
    expect_equal(nrow(df), 0)

    # Test with special characters in column names
    df <- initDataFrameWithColnames(c("Name", "Age#", "Gender@"))
    expect_true(is.data.frame(df))
    expect_equal(colnames(df), c("Name", "Age.", "Gender."))
    expect_equal(nrow(df), 0)

    # Test with duplicate column names
    df <- initDataFrameWithColnames(c("Name", "Name", "Gender"))
    expect_true(is.data.frame(df))
    expect_equal(colnames(df), c("Name", "Name.1", "Gender"))
    expect_equal(nrow(df), 0)
})


test_that("initEmptyTable works", {
    expect_length(initEmptyTable(), 0)
})

test_that("Convert a Table to Numeric works", {
    inputTable <- table(rep(letters[1:3], times = 1:3))
    expectedOutput <- c(a = 1, b = 2, c = 3)
    expect_equal(tableToNumeric(inputTable), expectedOutput)
})

test_that("Convert Named Numeric Vector to Table", {
    input <- c(a = 1, b = 2, c = 3)
    expected <- table(rep(letters[1:3], times = 1:3))
    expect_equal(namedNumericToTable(input), expected)
})

test_that("createHash works", {
    expect_identical(createHash(), hash::hash())
    expect_equal(as.list(createHash("a", 1)), list(a = 1))
    expect_equal(
        createHash(c("key1", "key2"), 1),
        hash::hash(key1 = 1, key2 = 1)
    )
})
