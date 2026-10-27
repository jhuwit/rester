
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
least that much data is needed. Data are preprocessed with `actipy`
(calibrated and resampled to 30Hz) using `ar_read()`, which also reads
`.cwa`, `.gt3x`, and `.bin` device files.

``` r
library(rester)
data = ar_example_data(n_hours = 3)
out = estimate_rest(data, context_window_shift = 16L)
out$linear_sleepstages
```
