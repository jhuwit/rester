# rester 0.1.0

* Initial version wrapping the AcceleRest model
  (<https://github.com/NielsRLorenzen/AcceleRest>).
* Added `ar_download_repo()`/`ar_extract_repo()` and `ar_download_weights()`
  to download and extract the model code and pretrained weights, and
  `ar_setup()` to do both.
* Added `estimate_rest()` to predict sleep stages and respiratory events.
* `.onLoad()` declares the Python requirements with `reticulate::py_require()`.
* Device files are read with `actiread::acti_read()`, and data are
  standardized, given a sample rate, and gap-filled with `actibase`.
  Compressed files the reader cannot handle are decompressed to a temporary
  file, which is deleted after reading.
* On macOS, `KMP_DUPLICATE_LIB_OK=TRUE` is set (if unset) so torch and
  OpenMP-using readers (e.g. GGIRread) can run in the same R session.
