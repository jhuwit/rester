# Python requirements for AcceleRest inference.  `actipy` reads and
# preprocesses device files (and data.frames) to 30Hz, gravity-calibrated data;
# `h5py` reads the project-native `.h5` format.
#
# torch 2.2.2 is the last release with Intel macOS wheels, and it needs
# numpy < 2 and Python < 3.13.  AcceleRest uses `torch.nn.RMSNorm` (torch
# >= 2.4), which `rester_predict.py` provides for older torch versions.
.intel_mac = "sys_platform == 'darwin' and platform_machine == 'x86_64'"
.not_intel_mac = "sys_platform != 'darwin' or platform_machine != 'x86_64'"
.rester_packages = c(
  paste0("torch>=2.4; ", .not_intel_mac),
  paste0("torch==2.2.2; ", .intel_mac),
  paste0("numpy<2; ", .intel_mac),
  "numpy",
  "pandas",
  "h5py",
  "actipy>=3.8.0"
)

# Python module names to check
.rester_modules = c(
  "torch",
  "numpy",
  "pandas",
  "h5py",
  "actipy"
)

.rester_python_version = ">=3.9,<3.13"

#' Command for `py_require` for `rester`
#'
#' Declares the Python packages needed to run AcceleRest using
#' [reticulate::py_require()].  This is called when `rester` is loaded, so
#' calling it directly is only needed if you pass additional arguments.
#'
#' @param ... arguments to pass to [reticulate::py_require()]
#'
#' @returns The output of [reticulate::py_require()], invisibly.
#' @export
py_require_rester = function(...) {
  reticulate::py_require(
    packages = .rester_packages,
    python_version = .rester_python_version,
    ...)
}

#' Check for the Python Modules Used by `rester`
#'
#' @returns A logical value indicating whether all Python modules needed
#' by `rester` can be found.
#' @export
#' @examples
#' \donttest{
#'   # Checking starts Python, which installs the Python packages, so this
#'   # runs only after ar_setup()
#'   if (ar_have_repo() && ar_have_weights()) {
#'     ar_python_modules_installed()
#'   }
#' }
ar_python_modules_installed = function() {
  .python_modules_available(.rester_modules)
}

# `reticulate::py_module_available()` imports each target module, and
# importing torch is needlessly expensive for an availability check.
# `importlib.util.find_spec()` only checks whether a module can be located on
# Python's import path.
.python_modules_available = function(modules) {
  tryCatch({
    importlib_util = reticulate::import("importlib.util")
    all(vapply(
      modules,
      function(module) !is.null(importlib_util$find_spec(module)),
      logical(1)
    ))
  }, error = function(error) FALSE)
}

# Import the bundled bridge module once per session.  `convert = FALSE` keeps
# large arrays in Python; results are converted explicitly.
.rester_env = new.env(parent = emptyenv())

rester_python = function() {
  if (is.null(.rester_env$py)) {
    py_require_rester()
    .rester_env$py = reticulate::import_from_path(
      "rester_predict",
      path = system.file("python", package = "rester"),
      convert = FALSE
    )
  }
  .rester_env$py
}
