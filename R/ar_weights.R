.ar_github = "https://github.com/NielsRLorenzen/AcceleRest"

# Commit of the AcceleRest code whose `hubconf.py` matches the v0.1.1 weights
.ar_default_ref = "9c57771e016141792b27bcf718b4b83885bbe6a0"
.ar_default_release = "v0.1.1"

#' `rester` Cache Directory
#'
#' The default location for the downloaded AcceleRest code and weights.  It
#' can be set using `options(rester.cache_dir = )` or the `RESTER_CACHE_DIR`
#' environment variable, and otherwise is
#' `tools::R_user_dir("rester", "cache")`.
#'
#' @returns A character path.  The directory is not created.
#' @export
#' @examples
#' ar_cache_dir()
ar_cache_dir = function() {
  dir = getOption("rester.cache_dir", Sys.getenv("RESTER_CACHE_DIR"))
  if (is.null(dir) || !nzchar(dir)) {
    dir = tools::R_user_dir("rester", which = "cache")
  }
  path.expand(dir)
}

#' AcceleRest Model Weights
#'
#' The pretrained weights released at
#' \url{https://github.com/NielsRLorenzen/AcceleRest/releases}.
#' The heads `"linear_sleepstages"`, `"lstm_sleepstages"`, and
#' `"linear_respevents"` are used for prediction; the autoencoder weights are
#' the pretrained (not fine-tuned) backbones.
#'
#' @param weights_dir directory where the weights are (to be) stored.
#' If `NULL`, the `file` column is just the file name.
#' @param release the GitHub release tag of the weights
#'
#' @returns A `data.frame` with the model name, file, URL, expected size
#' in bytes, and the class labels (for prediction heads).
#' @export
#' @examples
#' ar_weights()
ar_weights = function(weights_dir = NULL, release = .ar_default_release) {
  df = data.frame(
    model = c(
      "linear_sleepstages",
      "lstm_sleepstages",
      "linear_respevents",
      "autoencoder",
      "pulse_autoencoder"
    ),
    file = c(
      "accelerest_sleepstage_linear_weights.pt",
      "accelerest_sleepstage_lstmc_weights.pt",
      "accelerest_respevent_linear_weights.pt",
      "accelerest_autoencoder_weights.pt",
      "pulse_autoencoder_weights.pt"
    ),
    size = c(34424925, 34430482, 34422724, 37196892, 35345690)
  )
  df$url = paste0(.ar_github, "/releases/download/", release, "/", df$file)
  df$labels = I(list(
    .ar_sleep_labels, .ar_sleep_labels, .ar_resp_labels, NULL, NULL
  ))
  # Sizes are only known for the default release
  if (!identical(release, .ar_default_release)) {
    df$size = NA_real_
  }
  if (!is.null(weights_dir)) {
    weights_dir = normalizePath(
      path.expand(weights_dir), winslash = "/", mustWork = FALSE)
    df$file = file.path(weights_dir, df$file)
  }
  df
}

.ar_sleep_labels = c("deep", "light", "rem", "wake")
.ar_resp_labels = c("no_event", "event")
.ar_heads = c("linear_sleepstages", "lstm_sleepstages", "linear_respevents")

ar_match_models = function(model, choices = ar_weights()$model) {
  if (is.null(model)) {
    return(.ar_heads)
  }
  bad = setdiff(model, choices)
  if (length(bad) > 0) {
    stop("Unknown AcceleRest model(s): ", paste(bad, collapse = ", "),
         ". Choose from: ", paste(choices, collapse = ", "), call. = FALSE)
  }
  unique(model)
}

ar_check_weight_files = function(files) {
  ok = file.exists(files$file)
  known = ok & !is.na(files$size)
  ok[known] = file.size(files$file[known]) == files$size[known]
  ok
}

#' @rdname ar_weights
#' @param model the models to check or download.  Defaults to the
#' three prediction heads.
#' @param quiet If `FALSE`, prints a message if weights are missing
#' (`ar_have_weights`), or shows download progress (`ar_download_weights`).
#' @export
ar_have_weights = function(
    model = NULL,
    weights_dir = ar_cache_dir(),
    quiet = TRUE,
    release = .ar_default_release) {
  model = ar_match_models(model)
  files = ar_weights(weights_dir, release = release)
  files = files[files$model %in% model, , drop = FALSE]
  check = all(ar_check_weight_files(files))
  if (!check && !quiet) {
    message("Some AcceleRest weights are missing or incomplete in ",
            weights_dir, ". Run ar_download_weights().")
  }
  check
}

#' @rdname ar_weights
#' @param overwrite If `TRUE`, re-download files that already exist
#' @param ... additional arguments to pass to [curl::curl_download()]
#' @export
#' @examples
#' \donttest{
#'   weights_dir = file.path(tempdir(), "rester-weights")
#'   files = try(ar_download_weights("linear_sleepstages", weights_dir))
#' }
ar_download_weights = function(
    model = NULL,
    weights_dir = ar_cache_dir(),
    overwrite = FALSE,
    quiet = TRUE,
    release = .ar_default_release,
    ...) {
  model = ar_match_models(model)
  files = ar_weights(weights_dir, release = release)
  files = files[files$model %in% model, , drop = FALSE]
  dir.create(dirname(files$file[1]), recursive = TRUE, showWarnings = FALSE)

  need = overwrite | !ar_check_weight_files(files)
  for (i in which(need)) {
    # Download to a temporary file so an interrupted download does not
    # leave a truncated weights file in place.
    tfile = paste0(files$file[i], ".download")
    on.exit(unlink(tfile), add = TRUE)
    tryCatch(
      curl::curl_download(files$url[i], tfile, quiet = quiet, ...),
      error = function(error) {
        stop("Unable to download AcceleRest weights from ", files$url[i],
             ". ", conditionMessage(error), call. = FALSE)
      }
    )
    file.rename(tfile, files$file[i])
  }
  ok = ar_check_weight_files(files)
  if (!all(ok)) {
    stop("Downloaded AcceleRest weights are incomplete: ",
         paste(basename(files$file[!ok]), collapse = ", "), call. = FALSE)
  }
  files
}

#' Load AcceleRest Model Weights
#'
#' Extracts the `torch` state dictionary (the named parameter tensors) from a
#' downloaded weights file.
#'
#' @param model a single model name, see [ar_weights()]
#' @inheritParams ar_weights
#' @param as_r If `TRUE`, convert each tensor to an R array; otherwise
#' return the Python `OrderedDict` of tensors.
#'
#' @returns A named list of arrays or a Python object.
#' @export
#' @examples
#' \donttest{
#'   weights_dir = file.path(tempdir(), "rester-weights")
#'   if (ar_python_modules_installed() &&
#'       ar_have_weights("linear_sleepstages", weights_dir)) {
#'     sd = ar_load_weights("linear_sleepstages", weights_dir)
#'     names(sd)[1:5]
#'   }
#' }
ar_load_weights = function(
    model = "linear_sleepstages",
    weights_dir = ar_cache_dir(),
    as_r = TRUE) {
  assertthat::assert_that(assertthat::is.string(model))
  model = ar_match_models(model)
  files = ar_weights(weights_dir)
  file = files$file[files$model == model]
  if (!file.exists(file)) {
    stop("Weights not found at ", file, ". Run ar_download_weights().",
         call. = FALSE)
  }
  py = rester_python()
  state_dict = py$load_state_dict(file)
  if (as_r) {
    state_dict = lapply(
      reticulate::py_to_r(state_dict),
      function(x) reticulate::py_to_r(x$detach()$cpu()$numpy())
    )
  }
  state_dict
}
