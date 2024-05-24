# test_that("Initialize Testthat Files", {
#   # Create a temporary directory for testing
#   temp_dir <- tempdir()
#   testthat_dir <- file.path(temp_dir, "tests", "testthat")
#   r_dir <- file.path(temp_dir, "R")
#   dir.create(testthat_dir)
#   dir.create(r_dir)

#   # Create some sample R files
#   writeLines("", file.path(r_dir, "file1.R"))
#   writeLines("", file.path(r_dir, "file2.R"))
#   writeLines("", file.path(r_dir, "file3.R"))

#   # Test initialization without ignoring any files
#   initTestthat(rDir = r_dir, testDir = testthat_dir)
#   expect_true(file.exists(file.path(testthat_dir, "test-file1.R")))
#   expect_true(file.exists(file.path(testthat_dir, "test-file2.R")))
#   expect_true(file.exists(file.path(testthat_dir, "test-file3.R")))

#   # Test initialization with ignoring files
#   initTestthat(rDir = r_dir, testDir = testthat_dir, ignore = "^file1.R$")
#   expect_false(file.exists(file.path(testthat_dir, "test-file1.R")))
#   expect_true(file.exists(file.path(testthat_dir, "test-file2.R")))
#   expect_true(file.exists(file.path(testthat_dir, "test-file3.R")))

#   # Clean up
#   unlink(temp_dir, recursive = TRUE)
# })
