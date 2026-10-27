#' Download and Extract the AcceleRest Code
#'
#' Downloads the source archive of
#' \url{https://github.com/NielsRLorenzen/AcceleRest} at a fixed commit and
#' extracts the files needed to build the models (`hubconf.py` and the
#' `accelerest` Python package).  The code is downloaded rather than bundled
#' as the AcceleRest repository does not declare a license.
#'
#' @param repo_dir directory to extract the code into.  The code is placed
#' in `file.path(repo_dir, "AcceleRest")`.
#' @param ref git commit, tag, or branch of AcceleRest to download
#' @param overwrite If `TRUE`, re-download and replace an existing copy
#' @param quiet passed to [curl::curl_download()]
#' @param ... additional arguments to pass to [curl::curl_download()]
#'
#' @returns The path to the extracted repository.  `ar_have_repo` returns
#' a logical.
#' @export
#' @examples
#' \dontrun{
#'   # downloads the AcceleRest code to ar_cache_dir()
#'   path = ar_download_repo()
#' }
#' ar_have_repo()
ar_download_repo = function(
    repo_dir = ar_cache_dir(),
    ref = .ar_default_ref,
    overwrite = FALSE,
    quiet = TRUE,
    ...) {
  assertthat::assert_that(assertthat::is.string(ref))
  path = ar_repo_path(repo_dir)
  if (ar_have_repo(repo_dir) && !overwrite) {
    return(path)
  }

  zip_file = tempfile(fileext = ".zip")
  on.exit(unlink(zip_file), add = TRUE)
  url = paste0(.ar_github, "/archive/", ref, ".zip")
  tryCatch(
    curl::curl_download(url, zip_file, quiet = quiet, ...),
    error = function(error) {
      stop("Unable to download the AcceleRest code from ", url, ". ",
           conditionMessage(error), call. = FALSE)
    }
  )
  ar_extract_repo(zip_file, repo_dir = repo_dir)
}

#' @rdname ar_download_repo
#' @param zip_file a source archive of AcceleRest, e.g. downloaded from
#' GitHub
#' @export
ar_extract_repo = function(zip_file, repo_dir = ar_cache_dir()) {
  assertthat::assert_that(assertthat::is.readable(zip_file))
  path = ar_repo_path(repo_dir)

  files = utils::unzip(zip_file, list = TRUE)$Name
  # GitHub archives have a single top-level folder, e.g. AcceleRest-<ref>/
  relative = sub("^[^/]+/", "", files)
  keep = (relative == "hubconf.py" |
            grepl("^accelerest/.*[.]py$", relative)) &
    !grepl("__pycache__", relative)
  if (!any(relative[keep] == "hubconf.py")) {
    stop("hubconf.py not found in ", zip_file,
         "; is this an AcceleRest archive?", call. = FALSE)
  }

  exdir = tempfile("accelerest-")
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  utils::unzip(zip_file, files = files[keep], exdir = exdir)
  top = file.path(exdir, sub("/.*", "", files[keep][1]))

  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  src = file.path(top, c("hubconf.py", "accelerest"))
  file.copy(src[file.exists(src)], path, recursive = TRUE)
  if (!ar_have_repo(repo_dir)) {
    stop("Extraction of the AcceleRest code into ", path, " failed",
         call. = FALSE)
  }
  path
}

#' @rdname ar_download_repo
#' @export
ar_have_repo = function(repo_dir = ar_cache_dir()) {
  path = ar_repo_path(repo_dir)
  file.exists(file.path(path, "hubconf.py")) &&
    file.exists(file.path(path, "accelerest", "models", "roformer.py"))
}

ar_repo_path = function(repo_dir) {
  normalizePath(
    file.path(path.expand(repo_dir), "AcceleRest"),
    winslash = "/", mustWork = FALSE)
}

#' Download AcceleRest Code and Weights
#'
#' Runs [ar_download_repo()] and [ar_download_weights()].
#'
#' @param model the models to download, see [ar_weights()]
#' @param cache_dir directory to store the code and weights in
#' @param ... additional arguments to pass to [curl::curl_download()]
#' @inheritParams ar_download_repo
#'
#' @returns A list with the repository path and the weights `data.frame`.
#' @export
#' @examples
#' \dontrun{
#'   # downloads the AcceleRest code and ~100MB of weights to ar_cache_dir()
#'   ar_setup()
#' }
ar_setup = function(
    model = NULL,
    cache_dir = ar_cache_dir(),
    overwrite = FALSE,
    quiet = TRUE,
    ...) {
  list(
    repo_dir = ar_download_repo(
      repo_dir = cache_dir, overwrite = overwrite, quiet = quiet, ...),
    weights = ar_download_weights(
      model = model, weights_dir = cache_dir, overwrite = overwrite,
      quiet = quiet, ...)
  )
}
