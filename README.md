
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rester

<!-- badges: start -->

[![R-CMD-check](https://github.com/jhuwit/rester/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jhuwit/rester/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jhuwit/rester/graph/badge.svg)](https://app.codecov.io/gh/jhuwit/rester)
<!-- badges: end -->

The goal of `rester` is to estimate sleep stages and respiratory events
from wrist-worn accelerometry. It wraps the AcceleRest model from
<https://github.com/NielsRLorenzen/AcceleRest>, which predicts, for each
30-second epoch:

- sleep stages (deep, light, REM, wake) with a linear head
  (`"linear_sleepstages"`) or an LSTM head (`"lstm_sleepstages"`), and
- respiratory events (`"linear_respevents"`).

The Python requirements (`torch`, `actipy`, etc.) are declared with
`reticulate::py_require()` when the package is loaded, so `reticulate`
sets up the Python environment automatically.

## Installation

You can install the development version of rester from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jhuwit/rester")
```

## Downloading the model

The AcceleRest code and pretrained weights are downloaded (not bundled)
into `ar_cache_dir()`, which can be set with
`options(rester.cache_dir = )` or the `RESTER_CACHE_DIR` environment
variable:

``` r
library(rester)
ar_setup()
```

which runs `ar_download_repo()` (downloading and extracting the code)
and `ar_download_weights()`. The raw weights can be extracted with
`ar_load_weights()`.

## Example

AcceleRest uses context windows of 256 epochs (about 2.1 hours), so at
least that much data is needed. `estimate_rest()` takes a `data.frame`
or a file:

``` r
library(rester)
data = ar_example_data(n_hours = 3)
out = estimate_rest(data, context_window_shift = 16L)
out$linear_sleepstages

file = "/path/to/device_file.gt3x.gz"
out = estimate_rest(file, context_window_shift = 32L)
```

`context_window_shift = 1L` averages predictions over the most windows
but is much slower on a CPU.

## Reading and preprocessing

Data are read and preprocessed by `ar_read()`, which `estimate_rest()`
calls:

- Device files (`.cwa`, `.gt3x`, GENEActiv/Genea/Matrix `.bin`) are read
  with [`actiread`](https://github.com/jhuwit/actiread), CSV files with
  `readr`, and `.h5` files (already preprocessed, as described in the
  AcceleRest README) with `h5py`. Compressed files (`.gz`, `.bz2`,
  `.xz`) are read directly, or decompressed to a temporary file if the
  reader cannot handle them.
- Columns are standardized, the sample rate is found, and gaps
  (e.g. ActiGraph idle sleep mode) are filled with the last observation
  carried forward, as ActiLife does, using
  [`actibase`](https://github.com/jhuwit/actibase). Any gap left in the
  data makes the predictions for every window covering it missing.
- The data are calibrated and resampled to 30Hz with the `actipy` Python
  module. Use `detect_nonwear = TRUE` to set non-wear to `NA`; otherwise
  a device lying still off the wrist may be scored as sleep.

## Python setup

The Python requirements (`torch`, `actipy`, etc.) are declared with
`reticulate::py_require()` when the package is loaded. On Intel Macs,
the last available `torch` is 2.2.2, which `rester` supports. On macOS,
`rester` sets `KMP_DUPLICATE_LIB_OK=TRUE` (if unset) so `torch` and
OpenMP-using readers can run in the same R session. `py_estimate_rest()`
runs `estimate_rest()` in a separate R process.

## Testing

The tests use mocks and download nothing. To also run the real model,
which downloads the code, weights, and `torch`, set:

``` r
Sys.setenv(RESTER_FULL_TEST = "true", RESTER_TEST_CACHE_DIR = "/path/to/cache")
devtools::test()
```

On GitHub Actions, the full test runs on the `ubuntu-latest` (release)
job.
