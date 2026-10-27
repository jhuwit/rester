.onLoad <- function(libname, pkgname) {
  ar_allow_duplicate_openmp()
  # Only declares the requirements; nothing is installed or initialized until
  # Python is first used.
  py_require_rester()
}

# On macOS, torch ships its own OpenMP runtime (libiomp5 on Intel, libomp on
# Apple Silicon), and R device readers such as GGIRread (used by actiread for
# GENEActiv and Axivity files) link R's libomp.  Loading both aborts R with
# "OMP: Error #15" in either order.  Allowing the duplicate runtime gave
# identical predictions to running the reader and torch in separate
# processes.  A value set by the user is kept.
ar_allow_duplicate_openmp = function() {
  if (identical(Sys.info()[["sysname"]], "Darwin") &&
      !nzchar(Sys.getenv("KMP_DUPLICATE_LIB_OK"))) {
    Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
  }
}
