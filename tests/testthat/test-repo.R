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
