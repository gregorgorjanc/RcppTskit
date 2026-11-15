test_that("core operations work when reticulate is unavailable", {
  if (
    requireNamespace(
      "reticulate",
      quietly = TRUE
    )
  ) {
    skip("This test exercises installation without a supported reticulate.")
  }

  ts_file <- system.file("examples/test.trees", package = "RcppTskit")
  ts <- ts_load(ts_file)
  expect_equal(ts$num_individuals(), 8L)
  tc <- ts$dump_tables()
  expect_equal(tc$num_individuals(), ts$num_individuals())

  path <- tempfile(fileext = ".trees")
  on.exit(unlink(path), add = TRUE)
  ts$dump(path)
  restored <- ts_load(path)
  expect_equal(restored$num_nodes(), ts$num_nodes())
  expect_equal(restored$num_edges(), ts$num_edges())

  dependency_message <- "Python interoperability requires the R package 'reticulate'"
  result <- get_tskit_py()
  expect_s3_class(result, "simpleError")
  expect_match(conditionMessage(result), dependency_message)
  expect_message(expect_false(check_tskit_py(result)), dependency_message)
  expect_error(check_tskit_py(result, stop = TRUE), dependency_message)
  expect_error(ts$r_to_py(), dependency_message)
  expect_error(tc$r_to_py(), dependency_message)
  expect_error(ts_py_to_r(NULL), dependency_message)
  expect_error(tc_py_to_r(NULL), dependency_message)
})
