test_that("ar_weights lists release assets", {
  w <- ar_weights()
  expect_equal(
    w$model,
    c("linear_sleepstages", "lstm_sleepstages", "linear_respevents",
      "autoencoder", "pulse_autoencoder")
  )
  expect_true(all(grepl("^https://github.com/NielsRLorenzen/AcceleRest/releases/download/v0.1.1/",
                        w$url)))
  expect_equal(w$labels[[1]], c("deep", "light", "rem", "wake"))
  expect_equal(w$labels[[3]], c("no_event", "event"))

  dir <- file.path(tempdir(), "rester weights")
  expect_true(all(startsWith(ar_weights(dir)$file,
                             normalizePath(dir, winslash = "/", mustWork = FALSE))))
  expect_true(all(is.na(ar_weights(release = "v9")$size)))
})

test_that("unknown models are rejected", {
  expect_error(ar_have_weights("bad", tempdir()), "Unknown AcceleRest model")
})

test_that("ar_have_weights checks existence and size", {
  dir <- tempfile("rester-weights-")
  dir.create(dir)
  expect_false(ar_have_weights("linear_sleepstages", dir))
  expect_message(ar_have_weights("linear_sleepstages", dir, quiet = FALSE),
                 "missing")
  file <- ar_weights(dir)$file[1]
  writeBin(raw(10), file)
  expect_false(ar_have_weights("linear_sleepstages", dir))
  expect_true(ar_have_weights("linear_sleepstages", dir, release = "other"))
})

test_that("ar_download_weights downloads only missing files", {
  dir <- tempfile("rester-weights-")
  w <- ar_weights(dir)
  urls <- character()
  download <- function(url, destfile, ...) {
    urls <<- c(urls, url)
    writeBin(raw(1), destfile)
    destfile
  }
  get_weights <- function() {
    with_mocked_bindings(
      ar_download_weights(c("linear_sleepstages", "linear_respevents"), dir),
      curl_download = download,
      .package = "curl"
    )
  }
  files <- with_mocked_bindings({
    get_weights()
    get_weights()
  },
  ar_check_weight_files = function(files) file.exists(files$file),
  .package = "rester"
  )
  # second call reuses the existing files
  expect_equal(basename(urls), basename(w$file[c(1, 3)]))
  expect_true(all(file.exists(files$file)))
  expect_false(any(file.exists(paste0(files$file, ".download"))))
})

test_that("download failures give an informative error", {
  dir <- tempfile("rester-weights-")
  expect_error(
    with_mocked_bindings(
      ar_download_weights("linear_sleepstages", dir),
      curl_download = function(...) stop("offline"),
      .package = "curl"
    ),
    "Unable to download AcceleRest weights.*offline"
  )
})

test_that("ar_cache_dir uses the option, then the environment variable", {
  old_opt <- options(rester.cache_dir = NULL)
  old_env <- Sys.getenv("RESTER_CACHE_DIR", unset = NA)
  on.exit({
    options(old_opt)
    if (is.na(old_env)) Sys.unsetenv("RESTER_CACHE_DIR") else
      Sys.setenv(RESTER_CACHE_DIR = old_env)
  })
  Sys.unsetenv("RESTER_CACHE_DIR")
  expect_equal(ar_cache_dir(), tools::R_user_dir("rester", which = "cache"))
  Sys.setenv(RESTER_CACHE_DIR = "~/env-cache")
  expect_equal(ar_cache_dir(), path.expand("~/env-cache"))
  options(rester.cache_dir = "~/opt-cache")
  expect_equal(ar_cache_dir(), path.expand("~/opt-cache"))
})

test_that("NULL models default to the prediction heads", {
  expect_equal(rester:::ar_match_models(NULL),
               c("linear_sleepstages", "lstm_sleepstages", "linear_respevents"))
})

test_that("incomplete downloads are an error", {
  dir <- tempfile("rester-weights-")
  expect_error(
    with_mocked_bindings(
      ar_download_weights("linear_sleepstages", dir),
      curl_download = function(url, destfile, ...) writeBin(raw(1), destfile),
      .package = "curl"
    ),
    "incomplete"
  )
})

test_that("ar_load_weights extracts the state dictionary", {
  dir <- tempfile("rester-weights-")
  expect_error(ar_load_weights("linear_sleepstages", dir), "not found")
  dir.create(dir)
  file <- ar_weights(dir)$file[1]
  writeBin(raw(1), file)
  tensor <- function(value) {
    list(detach = function() list(cpu = function() list(
      numpy = function() value)))
  }
  fake_python <- list(load_state_dict = function(path) {
    expect_equal(path, file)
    list(a.weight = tensor(matrix(1:4, 2)), a.bias = tensor(1:2))
  })
  out <- with_mocked_bindings(
    ar_load_weights("linear_sleepstages", dir),
    rester_python = function() fake_python,
    .package = "rester"
  )
  expect_equal(out, list(a.weight = matrix(1:4, 2), a.bias = 1:2))

  raw_out <- with_mocked_bindings(
    ar_load_weights("linear_sleepstages", dir, as_r = FALSE),
    rester_python = function() fake_python,
    .package = "rester"
  )
  expect_named(raw_out, c("a.weight", "a.bias"))
  expect_error(ar_load_weights(c("linear_sleepstages", "lstm_sleepstages"), dir))
})
