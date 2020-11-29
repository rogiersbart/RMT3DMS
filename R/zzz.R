.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.RMT3DMS <- list(
    RMT3DMS.path = file.path(system.file(package = "RMT3DMS"), "code"),
    RMT3DMS.ui = ifelse(is.null(opts$RMODFLOW.ui), "verbose", opts$RMODFLOW.ui),
    RMT3DMS.theme = ifelse(is.null(opts$RMODFLOW.theme), "RMODFLOW", opts$RMODFLOW.theme)
  )
  toset <- !(names(opts.RMT3DMS) %in% names(opts))
  if(any(toset)) options(opts.RMT3DMS[toset])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  rui::alert("{{RMT3DMS}} is still in its experimental lifecycle stage.")
  rui::alert("Use at your own risk, and submit issues here:")
  rui::alert("{.url https://github.com/rogiersbart/RMT3DMS/issues}")
  invisible()
}

# options
# path: enable users to set the code installation directory
# ui: allow for a hierarchy of verbosity
#   verbose: everything
#   quiet: only warn and code output
#   silent: only code output
#   none: nothing
# theme: ggplot2 theme, scales and template
#   RMODFLOW: recommended theme, encouraging users to tweak things
#   ggplot2: ggplot2 default behaviour

# TODO: this has to enter documentation somewhere!