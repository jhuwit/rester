## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes for the reviewers

* The package wraps the AcceleRest model
  (<https://github.com/NielsRLorenzen/AcceleRest>; Lorenzen et al. 2026,
  <doi:10.64898/2026.01.28.26345056>) using 'reticulate'.  The Python
  requirements are only declared with `reticulate::py_require()` in
  `.onLoad()`; nothing is installed unless the user runs the model.

* The AcceleRest code and pretrained weights (about 100MB) are not bundled.
  They are downloaded only when the user calls `ar_setup()`,
  `ar_download_repo()`, or `ar_download_weights()`, into
  `tools::R_user_dir("rester", "cache")` by default (or a user-specified
  directory).  Download failures give an informative error.

* The examples for these three download functions are in `\dontrun{}`
  because they download about 100MB.  Examples that need the model check that
  it has already been downloaded (without starting Python), so they do not
  download or install anything during checks.

* The tests mock all downloads and Python calls, and do not start Python.
  A full test with the real model runs only when `RESTER_FULL_TEST=true`.

* On macOS, `.onLoad()` sets the environment variable
  `KMP_DUPLICATE_LIB_OK=TRUE`, if the user has not set it.  The macOS 'torch'
  builds and R packages such as 'GGIRread' each load an OpenMP runtime, which
  otherwise aborts the R session.  Predictions were identical to running the
  two in separate processes.

* The DOI <https://doi.org/10.64898/2026.01.28.26345056> may return 403 to
  automated URL checks (medRxiv blocks them); it resolves in a browser.
