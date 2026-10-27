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

test_that("ar_read reads CSV files with readr", {
  file <- tempfile(fileext = ".csv.gz")
  # write_csv() drops fractional seconds, so use 1Hz data
  time <- as.POSIXct("2020-01-01 10:00:00", tz = "UTC") + 0:9
  readr::write_csv(data.frame(time = time, X = 1, Y = 0, Z = 0), file)
  passed <- NULL
  with_mocked_bindings(
    ar_read(file),
    rester_python = function() list(
      prepare_dataframe = function(data, sample_rate, ...) {
        passed <<- list(data = data, sample_rate = sample_rate)
      }
    ),
    ar_prepared = function(out, tz) out,
    .package = "rester"
  ) |> suppressMessages()
  expect_equal(nrow(passed$data), 10)
  expect_equal(passed$sample_rate, 1)
})

test_that("ar_prepared splits the Python output", {
  out <- with_mocked_bindings(
    rester:::ar_prepared(list("array", "2020-01-01 10:00:00", list(a = 1)),
                         tz = "UTC"),
    py_get_item = function(x, i) x[[i + 1L]],
    py_to_r = function(x) x,
    .package = "reticulate"
  )
  expect_equal(out$accelerometry, "array")
  expect_equal(attr(out$start_time, "tzone"), "UTC")
  expect_equal(out$info, list(a = 1))
})

test_that("ar_device uses the requested or default device", {
  expect_equal(rester:::ar_device("cpu"), "cpu")
  expect_equal(
    with_mocked_bindings(
      rester:::ar_device(),
      rester_python = function() list(default_device = function() "cuda"),
      .package = "rester"
    ),
    "cuda"
  )
  expect_error(rester:::ar_device(1))
})

test_that("ar_check_assets reports missing code and weights", {
  dir <- tempfile("rester-")
  expect_error(rester:::ar_check_assets("linear_sleepstages", dir, dir),
               "code not found")
  expect_error(
    with_mocked_bindings(
      rester:::ar_check_assets("linear_sleepstages", dir, dir),
      ar_have_repo = function(...) TRUE,
      .package = "rester"
    ),
    "weights not found"
  )
})

test_that("ar_load_model loads the requested heads", {
  dir <- tempfile("rester-")
  passed <- NULL
  out <- with_mocked_bindings(
    ar_load_model(c("lstm_sleepstages", "linear_sleepstages"), dir, dir,
                  device = "cpu"),
    ar_check_assets = function(...) invisible(NULL),
    rester_python = function() list(
      load_model = function(repo_dir, weight_files, device) {
        passed <<- list(repo_dir = repo_dir, weight_files = weight_files,
                        device = device)
        "model"
      }
    ),
    .package = "rester"
  )
  expect_equal(out, "model")
  expect_equal(basename(passed$repo_dir), "AcceleRest")
  files <- passed$weight_files
  expect_named(files, c("lstm_sleepstages", "linear_sleepstages"))
  expect_equal(basename(unlist(files)),
               c("accelerest_sleepstage_lstmc_weights.pt",
                 "accelerest_sleepstage_linear_weights.pt"))
  expect_equal(passed$device, "cpu")
})

test_that("estimate_rest formats predictions for each head", {
  skip_if_not_installed("reticulate")
  prepared <- list(
    accelerometry = "array",
    start_time = structure("2020-01-01 22:00:00", tzone = "UTC"),
    info = list(SampleRate = 30)
  )
  probs <- list(
    linear_sleepstages = rbind(c(0.7, 0.1, 0.1, 0.1), c(0.1, 0.1, 0.1, 0.7)),
    linear_respevents = rbind(c(0.9, 0.1), c(0.2, 0.8))
  )
  passed <- NULL
  out <- with_mocked_bindings(
    estimate_rest(prepared, model = c("linear_sleepstages", "linear_respevents"),
                  context_window_shift = 8L, max_batch_size = 4L,
                  device = "cpu"),
    ar_check_assets = function(...) invisible(NULL),
    ar_read = function(...) stop("should not re-read prepared data"),
    ar_load_model = function(model, ...) model,
    rester_python = function() list(
      predict = function(net, accelerometry, context_window_shift,
                         max_batch_size, device) {
        passed <<- list(net = net, accelerometry = accelerometry,
                        shift = context_window_shift,
                        batch = max_batch_size, device = device)
        probs
      }
    ),
    .package = "rester"
  )
  expect_equal(passed$net, c("linear_sleepstages", "linear_respevents"))
  expect_equal(passed$shift, 8L)
  expect_equal(passed$batch, 4L)
  expect_named(out, c("linear_sleepstages", "linear_respevents"))
  expect_equal(as.character(out$linear_sleepstages$prediction),
               c("deep", "wake"))
  expect_equal(as.character(out$linear_respevents$prediction),
               c("no_event", "event"))
  expect_equal(format(out$linear_sleepstages$time, tz = "UTC"),
               c("2020-01-01 22:00:00", "2020-01-01 22:00:30"))
  expect_equal(attr(out, "info"), list(SampleRate = 30))
})

test_that("estimate_rest reads data and validates arguments", {
  expect_error(estimate_rest(data.frame(), context_window_shift = 0L))
  read <- FALSE
  with_mocked_bindings(
    estimate_rest("file.bin", model = "linear_respevents", device = "cpu"),
    ar_check_assets = function(...) invisible(NULL),
    ar_read = function(file, ...) {
      read <<- TRUE
      list(accelerometry = "array", start_time = NULL, info = NULL)
    },
    ar_load_model = function(model, ...) model,
    rester_python = function() list(
      predict = function(...) list(linear_respevents = rbind(c(0.9, 0.1)))
    ),
    .package = "rester"
  ) -> out
  expect_true(read)
  expect_true(is.na(out$linear_respevents$time))
})

test_that("py_estimate_rest runs estimate_rest via callr", {
  skip_if_not_installed("callr")
  fake_r <- function(func, args, show) do.call(func, args)
  calls <- character()
  out <- with_mocked_bindings(
    with_mocked_bindings(
      py_estimate_rest(file = "x"),
      r = fake_r,
      .package = "callr"
    ),
    py_require_rester = function(...) calls <<- c(calls, "py_require"),
    estimate_rest = function(...) list(...),
    .package = "rester"
  )
  expect_equal(out, list(file = "x"))
  expect_equal(calls, "py_require")

  # pyenv_function can modify the arguments
  out <- with_mocked_bindings(
    with_mocked_bindings(
      py_estimate_rest(file = "x",
                       pyenv_function = function(args) c(args, device = "cpu")),
      r = fake_r,
      .package = "callr"
    ),
    estimate_rest = function(...) list(...),
    .package = "rester"
  )
  expect_equal(out, list(file = "x", device = "cpu"))
})
