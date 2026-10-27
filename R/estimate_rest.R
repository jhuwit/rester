#' Read and Preprocess Data for AcceleRest
#'
#' AcceleRest expects 30Hz, gravity-calibrated data without gaps.  Device
#' files (`.cwa`, `.gt3x`, `.bin`, optionally compressed) are read with
#' [actiread::acti_read()], and CSV files with [readr::read_csv()].  Gaps
#' (all-zero rows from ActiGraph idle sleep mode, and missing values) are
#' filled with the last observation carried forward using
#' [actibase::acti_fill_zeros()], as ActiLife does.  The data are then
#' low-pass filtered, calibrated, and resampled to 30Hz with the `actipy`
#' Python module, using the same settings as the AcceleRest command-line
#' tool.  `.h5` files must already be preprocessed, with an
#' `axes x samples` array in `data/accelerometry`.
#'
#' @param file a `data.frame` with a time column and `X`, `Y`, `Z` axes (see
#' [actibase::acti_standardize_data()]), or a path to a device, CSV, or
#' `.h5` file, optionally compressed (`.gz`, `.bz2`, `.xz`).  Time stamps must be unique; data with time stamps rounded to
#' the second should have times reconstructed using the sample rate.
#' @param sample_rate sample rate (Hz) of the data.  If `NULL`, it is found
#' with [actibase::get_sample_rate()] (from the `sample_rate` attribute,
#' the header, or the time stamps).
#' @param fill_zeros If `TRUE`, fill gaps with
#' [actibase::acti_fill_zeros()].  Any gap left in the data makes the
#' predictions for every context window covering it (about 2.1 hours)
#' missing.
#' @param lowpass_hz,calibrate_gravity,detect_nonwear preprocessing options
#' passed to `actipy`.  Non-wear detected by `actipy` is set to `NA`.
#' @param verbose print diagnostic messages
#' @param ... additional arguments passed to [actiread::acti_read()] for
#' device files
#'
#' @returns A list with the Python (`numpy`) array of `3 x samples` data
#' (`accelerometry`), the time of the first sample (`start_time`, `NULL`
#' for `.h5` files), and `actipy` information about the data (`info`).
#' @export
#' @examples
#' \donttest{
#'   # Runs only after ar_setup(), so Python is not set up otherwise
#'   if (ar_have_repo() && ar_have_weights() &&
#'       ar_python_modules_installed()) {
#'     data = ar_example_data(n_hours = 0.1)
#'     out = ar_read(data)
#'     out$start_time
#'   }
#' }
ar_read = function(
    file,
    sample_rate = NULL,
    fill_zeros = TRUE,
    lowpass_hz = 15,
    calibrate_gravity = TRUE,
    detect_nonwear = FALSE,
    verbose = FALSE,
    ...) {
  assertthat::assert_that(assertthat::is.flag(fill_zeros))

  if (is.character(file)) {
    assertthat::assert_that(assertthat::is.string(file),
                            assertthat::is.readable(file))
    path = normalizePath(path.expand(file), winslash = "/", mustWork = TRUE)
    type = ar_file_type(path)
    compression = ar_compression(path)
    if (!is.na(compression) &&
        !compression %in% .ar_reader_compression[[type]]) {
      decompressed = actiread::acti_decompress_file(path)
      if (decompressed$temporary) {
        on.exit(unlink(decompressed$path), add = TRUE)
      }
      path = decompressed$path
    }
    if (type == "h5") {
      return(list(
        accelerometry = rester_python()$read_h5(path),
        start_time = NULL,
        info = NULL
      ))
    }
    file = if (type == "csv") {
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    } else {
      actiread::acti_read(path, ..., verbose = verbose)
    }
  }

  sample_rate = actibase::get_sample_rate(file, sample_rate = sample_rate)
  assertthat::assert_that(assertthat::is.number(sample_rate), sample_rate > 0)
  data = actibase::acti_standardize_data(file)
  rm(file)
  if (anyDuplicated(data$time) > 0) {
    stop("Time stamps in data are not unique.  If times are rounded, ",
         "reconstruct them using the sample rate, e.g. ",
         "time[1] + (seq_along(time) - 1) / sample_rate", call. = FALSE)
  }
  if (fill_zeros) {
    data = actibase::acti_fill_zeros(data)
  }
  # time, x, y, z for actipy
  data = actibase::acti_standardize_data(data, lower_case = TRUE)
  py = rester_python()
  tz = ar_time_zone(data$time)
  # Pass clock times as text so pandas gets naive times in the data's own
  # time zone (microsecond precision keeps high sample rates regular).
  if (inherits(data$time, "POSIXt")) {
    data$time = format(data$time, "%Y-%m-%d %H:%M:%OS6")
  }
  out = py$prepare_dataframe(
    data, as.numeric(sample_rate),
    lowpass_hz = lowpass_hz,
    calibrate_gravity = calibrate_gravity,
    detect_nonwear = detect_nonwear,
    resample_hz = 30L,
    verbose = verbose
  )
  ar_prepared(out, tz = tz)
}

# Compression formats each reader handles itself (actiread for device
# files, readr for CSV, h5py for .h5).  Other compressed files are
# decompressed to a temporary file, which is deleted after reading.
.ar_reader_compression = list(
  device = c("gz", "bz2", "xz"),
  csv = c("gz", "bz2", "xz"),
  h5 = character(0)
)

ar_compression = function(path) {
  ext = tolower(tools::file_ext(path))
  if (ext %in% c("gz", "bz2", "xz")) ext else NA_character_
}

ar_file_type = function(path) {
  ext = tolower(tools::file_ext(sub("[.](gz|bz2|xz)$", "", path,
                                    ignore.case = TRUE)))
  switch(ext, h5 = "h5", csv = "csv", "device")
}

ar_prepared = function(out, tz = "") {
  out = list(
    accelerometry = reticulate::py_get_item(out, 0L),
    start_time = reticulate::py_to_r(reticulate::py_get_item(out, 1L)),
    info = reticulate::py_to_r(reticulate::py_get_item(out, 2L))
  )
  attr(out$start_time, "tzone") = tz
  out
}

# actipy works on naive clock times, so times are given back in the data's
# own time zone.
ar_time_zone = function(time) {
  tz = attr(time, "tzone")
  if (is.null(tz) || !nzchar(tz[1])) "" else tz[1]
}

#' Load the AcceleRest Model
#'
#' Builds the multi-head AcceleRest model from downloaded code and weights
#' (see [ar_setup()]).  All heads share a single encoder backbone.
#'
#' @param model the prediction heads to include, one or more of
#' `"linear_sleepstages"`, `"lstm_sleepstages"`, and `"linear_respevents"`.
#' Defaults to all three.
#' @param repo_dir directory with the downloaded code, see
#' [ar_download_repo()]
#' @param weights_dir directory with the downloaded weights, see
#' [ar_download_weights()]
#' @param device `torch` device, e.g. `"cpu"`, `"cuda"`, or `"mps"`.  If
#' `NULL`, uses `"cuda"` if available, otherwise `"cpu"`.
#'
#' @returns A Python `torch` module.
#' @export
#' @examples
#' \donttest{
#'   # Runs only after ar_setup(), so Python is not set up otherwise
#'   if (ar_have_repo() && ar_have_weights("linear_sleepstages") &&
#'       ar_python_modules_installed()) {
#'     model = ar_load_model("linear_sleepstages")
#'   }
#' }
ar_load_model = function(
    model = NULL,
    repo_dir = ar_cache_dir(),
    weights_dir = ar_cache_dir(),
    device = NULL) {
  model = ar_match_models(model, choices = .ar_heads)
  ar_check_assets(model, repo_dir = repo_dir, weights_dir = weights_dir)
  py = rester_python()
  device = ar_device(device)
  files = ar_weights(weights_dir)
  files = files[match(model, files$model), , drop = FALSE]
  weight_files = reticulate::dict(stats::setNames(as.list(files$file), model))
  py$load_model(ar_repo_path(repo_dir), weight_files, device = device)
}

ar_check_assets = function(model, repo_dir, weights_dir) {
  if (!ar_have_repo(repo_dir)) {
    stop("AcceleRest code not found in ", repo_dir,
         ". Run ar_download_repo() or ar_setup().", call. = FALSE)
  }
  if (!ar_have_weights(model, weights_dir)) {
    stop("AcceleRest weights not found in ", weights_dir,
         ". Run ar_download_weights() or ar_setup().", call. = FALSE)
  }
}

ar_device = function(device = NULL) {
  if (is.null(device)) {
    device = reticulate::py_to_r(rester_python()$default_device())
  }
  assertthat::assert_that(assertthat::is.string(device))
  device
}

#' Estimate Sleep Stages and Respiratory Events with AcceleRest
#'
#' Runs the AcceleRest model on 30-second epochs of wrist-worn accelerometry.
#' Predictions are made over context windows of 256 epochs (about 2.1
#' hours), so at least that much data is needed; predictions for an epoch
#' are averaged across the overlapping windows covering it.
#'
#' @param file a `data.frame` or file path, see [ar_read()], or the output
#' of [ar_read()]
#' @inheritParams ar_load_model
#' @param cache_dir directory with the downloaded code and weights, see
#' [ar_setup()]
#' @param context_window_shift number of 30-second epochs to shift
#' consecutive context windows.  Smaller is slower but averages over more
#' windows.
#' @param max_batch_size number of context windows per forward pass.
#' Higher is faster but uses more memory.
#' @param ... additional arguments passed to [ar_read()]
#'
#' @returns A named list with a `tibble` for each head, with the epoch
#' number, start `time` of the epoch (`NA` for `.h5` files), the
#' probability of each class, and the most probable class (`prediction`).
#' Epochs with no valid data have `NA` values.  Sleep stage classes are
#' deep, light, rem, and wake; respiratory event classes are no_event and
#' event.  The `actipy` information about the data is in `attr(x, "info")`.
#' @references Lorenzen NR, Brink-Kjaer A, Jennum PJ, Mignot E (2026).
#' AcceleRest: A Physiology-Aware Masked Autoencoder for Wrist
#' Accelerometer-based Sleep Staging and Apnea Evaluation. *medRxiv*.
#' \doi{10.64898/2026.01.28.26345056}
#' @export
#' @examples
#' \donttest{
#'   # Runs only after ar_setup(), so Python is not set up otherwise
#'   if (ar_have_repo() && ar_have_weights("linear_sleepstages") &&
#'       ar_python_modules_installed()) {
#'     data = ar_example_data(n_hours = 2.5)
#'     out = estimate_rest(data, model = "linear_sleepstages",
#'                         context_window_shift = 32L)
#'     head(out$linear_sleepstages)
#'   }
#' }
estimate_rest = function(
    file,
    model = NULL,
    cache_dir = ar_cache_dir(),
    repo_dir = cache_dir,
    weights_dir = cache_dir,
    context_window_shift = 1L,
    max_batch_size = 16L,
    device = NULL,
    ...) {
  assertthat::assert_that(
    assertthat::is.count(context_window_shift),
    assertthat::is.count(max_batch_size)
  )
  model = ar_match_models(model, choices = .ar_heads)
  # fail before spending time reading the data
  ar_check_assets(model, repo_dir = repo_dir, weights_dir = weights_dir)

  if (!(is.list(file) && !is.data.frame(file) &&
        "accelerometry" %in% names(file))) {
    file = ar_read(file, ...)
  }
  device = ar_device(device)
  net = ar_load_model(model, repo_dir = repo_dir, weights_dir = weights_dir,
                      device = device)

  preds = rester_python()$predict(
    net,
    file$accelerometry,
    context_window_shift = as.integer(context_window_shift),
    max_batch_size = as.integer(max_batch_size),
    device = device
  )
  preds = reticulate::py_to_r(preds)

  start_time = file$start_time
  if (!is.null(start_time)) {
    start_time = as.POSIXct(start_time, tz = attr(start_time, "tzone"))
  }
  labels = ar_weights()
  result = lapply(model, function(name) {
    ar_format_predictions(
      preds[[name]],
      labels = labels$labels[[which(labels$model == name)]],
      start_time = start_time
    )
  })
  names(result) = model
  attr(result, "info") = file$info
  result
}

ar_format_predictions = function(probs, labels, start_time = NULL) {
  probs = matrix(probs, ncol = length(labels))
  colnames(probs) = paste0("prob_", labels)
  epoch = seq_len(nrow(probs))
  time = if (is.null(start_time)) {
    rep(as.POSIXct(NA), length(epoch))
  } else {
    start_time + (epoch - 1) * 30
  }
  prediction = rep(NA_character_, nrow(probs))
  valid = stats::complete.cases(probs)
  prediction[valid] = labels[max.col(probs[valid, , drop = FALSE],
                                     ties.method = "first")]
  dplyr::bind_cols(
    dplyr::tibble(epoch = epoch, time = time),
    dplyr::as_tibble(probs),
    dplyr::tibble(prediction = factor(prediction, levels = labels))
  )
}

#' Example Accelerometry Data
#'
#' Reads the example 12 hours of 30Hz wrist accelerometry data included in the
#' package (the same recording as in the `sleeper` package), reconstructing
#' the time stamps (stored rounded to the second) from the sample rate.
#'
#' @param n_hours number of hours of data to read (up to 12).  `NULL`
#' reads all of it.
#'
#' @returns A `tibble` with columns `time`, `X`, `Y`, and `Z`, with the
#' `sample_rate` attribute set to 30.
#' @export
#' @examples
#' data = ar_example_data(n_hours = 0.01)
#' head(data)
ar_example_data = function(n_hours = NULL) {
  file = system.file("extdata", "example_data.csv.gz", package = "rester")
  n_max = if (is.null(n_hours)) Inf else ceiling(n_hours * 3600 * 30)
  data = readr::read_csv(file, n_max = n_max, show_col_types = FALSE,
                         progress = FALSE)
  data = actibase::acti_standardize_data(data)
  data$time = data$time[1] + (seq_len(nrow(data)) - 1) / 30
  attr(data, "sample_rate") = 30
  data
}

#' Run `estimate_rest` with Python in a Separate Process
#'
#' @param ... arguments to pass to [estimate_rest()]
#' @param pyenv_function function that sets up Python in the child process.
#' By default, it uses [rester::py_require_rester()].
#' If this function has an `args` argument, the output of
#' `pyenv_function` will be re-assigned to `args`.
#' @param show Logical, whether to show the standard output on the
#' screen while the child process is running, passed to [callr::r()]
#'
#' @returns Output from [rester::estimate_rest()].
#' @export
py_estimate_rest = function(
    ...,
    pyenv_function = function() {
      rester::py_require_rester()
    },
    show = FALSE) {
  rlang::check_installed("callr")
  callr::r(
    show = show,
    func = function(..., pyenv_function) {
      args = list(...)
      if ("args" %in% methods::formalArgs(pyenv_function)) {
        args = pyenv_function(args)
      } else {
        pyenv_function()
      }
      do.call(rester::estimate_rest, args = args)
    },
    args = list(...,
                pyenv_function = pyenv_function)
  )
}
