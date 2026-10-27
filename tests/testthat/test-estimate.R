test_that("ar_read standardizes, fills gaps, and passes clock times", {
  time <- as.POSIXct("2020-01-01 22:00:00", tz = "America/New_York") +
    (0:5) / 10
  data <- data.frame(HEADER_TIMESTAMP = time,
                     X = c(1, 0, NA, 2, 0, 3),
                     Y = c(1, 0, NA, 2, 0, 3),
                     Z = c(1, 0, NA, 2, 0, 3))
  passed <- NULL
  fake_python <- list(
    prepare_dataframe = function(data, sample_rate, ...) {
      passed <<- list(data = data, sample_rate = sample_rate, args = list(...))
      "prepared"
    }
  )
  out <- with_mocked_bindings(
    ar_read(data),
    rester_python = function() fake_python,
    ar_prepared = function(out, tz) list(out = out, tz = tz),
    .package = "rester"
  )
  expect_equal(out, list(out = "prepared", tz = "America/New_York"))
  expect_named(passed$data, c("time", "x", "y", "z"))
  expect_equal(passed$data$x, c(1, 1, 1, 2, 2, 3))
  expect_equal(passed$data$time[1], "2020-01-01 22:00:00.000000")
  expect_equal(passed$sample_rate, 10)
  expect_equal(passed$args$resample_hz, 30L)

  with_mocked_bindings(
    ar_read(data, fill_zeros = FALSE, sample_rate = 10),
    rester_python = function() fake_python,
    ar_prepared = function(out, tz) out,
    .package = "rester"
  )
  expect_equal(passed$data$x, c(1, 0, NA, 2, 0, 3))
})

test_that("ar_read rejects duplicated time stamps", {
  data <- data.frame(time = rep(Sys.time(), 2), X = 0, Y = 0, Z = 1)
  expect_error(
    with_mocked_bindings(
      ar_read(data, sample_rate = 30),
      rester_python = function() list(),
      .package = "rester"
    ),
    "not unique"
  )
})

test_that("ar_example_data reconstructs unique 30Hz times", {
  data <- ar_example_data(n_hours = 0.01)
  expect_named(data, c("time", "X", "Y", "Z"))
  expect_equal(nrow(data), 0.01 * 3600 * 30)
  expect_equal(anyDuplicated(data$time), 0L)
  expect_equal(attr(data, "sample_rate"), 30)
})

test_that("ar_format_predictions labels epochs", {
  probs <- rbind(c(0.1, 0.2, 0.3, 0.4), c(0.7, 0.1, 0.1, 0.1), NA)
  start <- as.POSIXct("2020-01-01 22:00:00", tz = "UTC")
  out <- rester:::ar_format_predictions(probs, rester:::.ar_sleep_labels, start)
  expect_named(out, c("epoch", "time", "prob_deep", "prob_light", "prob_rem",
                      "prob_wake", "prediction"))
  expect_equal(as.character(out$prediction), c("wake", "deep", NA))
  expect_equal(out$time, start + c(0, 30, 60))
  expect_true(all(is.na(
    rester:::ar_format_predictions(probs, rester:::.ar_sleep_labels)$time)))
})

test_that("compressed files are decompressed only if the reader cannot", {
  expect_equal(rester:::ar_compression("a/b.bin.xz"), "xz")
  expect_true(is.na(rester:::ar_compression("a/b.cwa")))
  expect_equal(rester:::ar_file_type("a/b.bin.xz"), "device")
  expect_equal(rester:::ar_file_type("a/b.CSV.gz"), "csv")
  expect_equal(rester:::ar_file_type("a/b.h5.bz2"), "h5")

  # h5py cannot read compressed files: decompress to a temporary file
  h5 <- tempfile(fileext = ".h5")
  writeLines("fake", h5)
  h5_xz <- paste0(h5, ".xz")
  con <- xzfile(h5_xz, "wb")
  writeLines("fake", con)
  close(con)
  read_path <- NULL
  fake_python <- list(read_h5 = function(path) {
    read_path <<- path
    expect_equal(readLines(path), "fake")
    "array"
  })
  out <- with_mocked_bindings(
    ar_read(h5_xz),
    rester_python = function() fake_python,
    .package = "rester"
  )
  expect_equal(out$accelerometry, "array")
  expect_false(identical(read_path, normalizePath(h5_xz, winslash = "/")))
  expect_match(read_path, "[.]h5$")
  expect_false(file.exists(read_path))

  # actiread reads compressed device files itself
  bin_xz <- sub("[.]h5[.]xz$", ".bin.xz", h5_xz)
  file.copy(h5_xz, bin_xz)
  device_path <- NULL
  data <- data.frame(time = Sys.time() + (0:2) / 10, X = 1, Y = 0, Z = 0)
  with_mocked_bindings(
    with_mocked_bindings(
      ar_read(bin_xz, sample_rate = 10),
      acti_read = function(path, ...) {
        device_path <<- path
        data
      },
      .package = "actiread"
    ),
    rester_python = function() list(prepare_dataframe = function(...) NULL),
    ar_prepared = function(out, tz) out,
    .package = "rester"
  )
  expect_equal(device_path, normalizePath(bin_xz, winslash = "/"))
})

test_that("ar_load_model requires downloaded code and weights", {
  dir <- tempfile("rester-")
  expect_error(ar_load_model("linear_sleepstages", dir, dir), "ar_download_repo")
  expect_error(ar_load_model("autoencoder", dir, dir), "Unknown AcceleRest")
})

test_that("estimate_rest runs end to end when Python and weights are present", {
  skip_on_cran()
  cache_dir <- Sys.getenv("RESTER_TEST_CACHE_DIR")
  if (!nzchar(cache_dir)) {
    skip("Set RESTER_TEST_CACHE_DIR to a directory with ar_setup() output")
  }
  skip_if_not(ar_python_modules_installed(), "Python modules not available")
  skip_if_not(ar_have_repo(cache_dir) &&
                ar_have_weights("linear_sleepstages", cache_dir))

  data <- ar_example_data(n_hours = 2.5)
  out <- estimate_rest(data, model = "linear_sleepstages",
                       cache_dir = cache_dir, context_window_shift = 32L,
                       device = "cpu")
  pred <- out$linear_sleepstages
  expect_true(nrow(pred) >= 256)
  probs <- as.matrix(pred[, grepl("^prob_", names(pred))])
  expect_equal(unname(rowSums(probs)), rep(1, nrow(probs)), tolerance = 1e-5)
  expect_equal(pred$time[1], data$time[1], tolerance = 1)
})
