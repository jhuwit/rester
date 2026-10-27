make_archive <- function(top = "AcceleRest-abc") {
  src <- tempfile("archive-src-")
  dir.create(file.path(src, top, "accelerest", "models", "__pycache__"),
             recursive = TRUE)
  dir.create(file.path(src, top, "notebooks"))
  writeLines("# hub", file.path(src, top, "hubconf.py"))
  writeLines("", file.path(src, top, "accelerest", "__init__.py"))
  writeLines("# model", file.path(src, top, "accelerest", "models", "roformer.py"))
  writeLines("", file.path(src, top, "accelerest", "models", "__pycache__", "x.py"))
  writeLines("{}", file.path(src, top, "notebooks", "nb.ipynb"))
  zip_file <- tempfile(fileext = ".zip")
  withr_dir <- getwd()
  on.exit(setwd(withr_dir))
  setwd(src)
  utils::zip(zip_file, files = top, flags = "-qr")
  zip_file
}

test_that("ar_extract_repo extracts only the model code", {
  skip_if(!nzchar(Sys.which("zip")), "zip not available")
  repo_dir <- tempfile("rester-repo-")
  expect_false(ar_have_repo(repo_dir))
  path <- ar_extract_repo(make_archive(), repo_dir)
  expect_true(ar_have_repo(repo_dir))
  expect_equal(basename(path), "AcceleRest")
  files <- list.files(path, recursive = TRUE)
  expect_setequal(files, c("hubconf.py", "accelerest/__init__.py",
                           "accelerest/models/roformer.py"))
})

test_that("ar_extract_repo rejects other archives", {
  skip_if(!nzchar(Sys.which("zip")), "zip not available")
  src <- tempfile()
  dir.create(file.path(src, "top"), recursive = TRUE)
  writeLines("x", file.path(src, "top", "README.md"))
  zip_file <- tempfile(fileext = ".zip")
  old <- setwd(src)
  on.exit(setwd(old))
  utils::zip(zip_file, files = "top", flags = "-qr")
  expect_error(ar_extract_repo(zip_file, tempfile()), "hubconf.py not found")
})

test_that("ar_download_repo uses the pinned archive and reuses existing code", {
  skip_if(!nzchar(Sys.which("zip")), "zip not available")
  repo_dir <- tempfile("rester-repo-")
  archive <- make_archive()
  urls <- character()
  download <- function(url, destfile, ...) {
    urls <<- c(urls, url)
    file.copy(archive, destfile, overwrite = TRUE)
    destfile
  }
  with_mocked_bindings({
    ar_download_repo(repo_dir)
    ar_download_repo(repo_dir)
  }, curl_download = download, .package = "curl")
  expect_equal(
    urls,
    paste0("https://github.com/NielsRLorenzen/AcceleRest/archive/",
           rester:::.ar_default_ref, ".zip")
  )
  expect_true(ar_have_repo(repo_dir))
})

test_that("repository download failures give an informative error", {
  expect_error(
    with_mocked_bindings(
      ar_download_repo(tempfile("rester-repo-")),
      curl_download = function(...) stop("offline"),
      .package = "curl"
    ),
    "Unable to download the AcceleRest code.*offline"
  )
})

test_that("ar_extract_repo errors if the extracted code is incomplete", {
  skip_if(!nzchar(Sys.which("zip")), "zip not available")
  src <- tempfile()
  dir.create(file.path(src, "top"), recursive = TRUE)
  writeLines("# hub", file.path(src, "top", "hubconf.py"))
  zip_file <- tempfile(fileext = ".zip")
  old <- setwd(src)
  on.exit(setwd(old))
  utils::zip(zip_file, files = "top", flags = "-qr")
  expect_error(ar_extract_repo(zip_file, tempfile()), "failed")
})

test_that("ar_setup downloads the code and weights into one directory", {
  dir <- tempfile("rester-")
  out <- with_mocked_bindings(
    ar_setup("linear_sleepstages", cache_dir = dir, quiet = TRUE),
    ar_download_repo = function(repo_dir, ...) repo_dir,
    ar_download_weights = function(model, weights_dir, ...) {
      list(model = model, weights_dir = weights_dir)
    },
    .package = "rester"
  )
  expect_equal(out$repo_dir, dir)
  expect_equal(out$weights, list(model = "linear_sleepstages",
                                 weights_dir = dir))
})
