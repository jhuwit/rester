# Full test with the real code, weights, and Python stack.  This downloads
# the AcceleRest code and weights (about 100MB) and the Python packages, so it
# only runs when RESTER_FULL_TEST=true (e.g. on one GitHub Actions job).
# The download goes to RESTER_TEST_CACHE_DIR if set, so it can be cached.
skip_if_not_full_test <- function() {
  if (!identical(tolower(Sys.getenv("RESTER_FULL_TEST")), "true")) {
    skip("Set RESTER_FULL_TEST=true to run the full test")
  }
}

full_test_cache_dir <- function() {
  dir <- Sys.getenv("RESTER_TEST_CACHE_DIR")
  if (!nzchar(dir)) {
    dir <- file.path(tempdir(), "rester-full-test")
  }
  dir
}

test_that("the full AcceleRest pipeline runs on real data", {
  skip_if_not_full_test()
  cache_dir <- full_test_cache_dir()
  ar_setup(cache_dir = cache_dir)
  expect_true(ar_have_repo(cache_dir))
  expect_true(ar_have_weights(weights_dir = cache_dir))
  # In the full test, missing Python modules are a failure, not a skip
  expect_true(ar_python_modules_installed())

  state_dict <- ar_load_weights("linear_sleepstages", cache_dir)
  expect_true(length(state_dict) > 100)
  expect_equal(dim(state_dict$patch_embedding.embed.weight), c(256L, 2700L))

  # actiread device reader on a compressed example file
  gt3x <- ar_read(actiread::acti_example_gt3x())
  expect_true(reticulate::py_to_r(gt3x$accelerometry$shape[[0]]) == 3L)

  data <- ar_example_data(n_hours = 2.5)
  out <- estimate_rest(data, cache_dir = cache_dir,
                       context_window_shift = 32L, device = "cpu")
  expect_named(out, c("linear_sleepstages", "lstm_sleepstages",
                      "linear_respevents"))
  for (pred in out) {
    expect_true(nrow(pred) >= 256)
    probs <- as.matrix(pred[, grepl("^prob_", names(pred))])
    expect_equal(unname(rowSums(probs)), rep(1, nrow(probs)), tolerance = 1e-5)
    expect_false(anyNA(pred$prediction))
  }
  expect_equal(as.numeric(out$linear_sleepstages$time[1]),
               as.numeric(data$time[1]), tolerance = 1)
})
