test_that("py_require_rester declares the Python requirements", {
  expect_true(with_mocked_bindings(
    py_require_rester(),
    py_require = function(packages, python_version, ...) {
      expect_equal(python_version, ">=3.9,<3.13")
      expect_true(any(startsWith(packages, "torch>=2.4;")))
      expect_true("actipy>=3.8.0" %in% packages)
      TRUE
    },
    .package = "reticulate"
  ))
})

test_that("the load hook registers the Python requirements", {
  expect_true(with_mocked_bindings(
    rester:::.onLoad("unused", "rester"),
    py_require_rester = function(...) TRUE,
    .package = "rester"
  ))
})

test_that("duplicate OpenMP runtimes are allowed only on macOS when unset", {
  old <- Sys.getenv("KMP_DUPLICATE_LIB_OK", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("KMP_DUPLICATE_LIB_OK") else
    Sys.setenv(KMP_DUPLICATE_LIB_OK = old))
  Sys.unsetenv("KMP_DUPLICATE_LIB_OK")
  rester:::ar_allow_duplicate_openmp()
  expected <- if (Sys.info()[["sysname"]] == "Darwin") "TRUE" else ""
  expect_equal(Sys.getenv("KMP_DUPLICATE_LIB_OK"), expected)
  Sys.setenv(KMP_DUPLICATE_LIB_OK = "FALSE")
  rester:::ar_allow_duplicate_openmp()
  expect_equal(Sys.getenv("KMP_DUPLICATE_LIB_OK"), "FALSE")
})

test_that("module check uses find_spec on module names", {
  expect_true(with_mocked_bindings(
    ar_python_modules_installed(),
    .python_modules_available = function(modules) {
      expect_equal(modules, c("torch", "numpy", "pandas", "h5py", "actipy"))
      TRUE
    },
    .package = "rester"
  ))
  importlib_util = list(
    find_spec = function(module) if (module == "missing") NULL else list()
  )
  expect_false(with_mocked_bindings(
    rester:::.python_modules_available(c("numpy", "missing")),
    import = function(module, ...) importlib_util,
    .package = "reticulate"
  ))
})
