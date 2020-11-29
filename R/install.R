#' Install external codes
#' 
#' This function installs external codes that are used by [RMT3DMS]. 
#' 
#' ## Supported software
#' [MT3DMS v5.3](https://hydro.geo.ua.edu/) and [MT3D-USGS](https://www.usgs.gov/software/mt3d-usgs-groundwater-solute-transport-simulator-modflow). 
#' The zip files with windows binaries hosted at these website
#' are downloaded and extracted in the installation directory. The main
#' folder names are modified in order to have more consistency.
#' 
#' ## Installation location
#' The default installation location is `file.path(system.file(package =
#' "RMT3DMS"), "code")`, but it can be altered by setting option
#' `RMT3DMS.path`.
#'
#' @param code Character vector with the codes to install, or `"all"` (default).
#' @param overwrite Logical. Overwrite when the code is already installed? If
#'   `NULL` (default), the user is asked what to do in an interactive session.
#'   An error message is issued otherwise.
#' @export
#' @examples
#' rmt_install() # Install all codes.
#' rmt_install("MT3D-USGS", overwrite = TRUE) # Install MT3D-USGS.
#' rmt_install("MT3DMS", overwrite = TRUE) # Install MT3DMS.

rmt_install <- function(code = "all", overwrite = NULL) {
  if (code[1] == "all") {
    rmti_install_code(rmtd_supported_codes, overwrite = overwrite)
    return(invisible())
  }
  codes <- rmtd_supported_codes %>% c(stringr::str_remove(., "MT3D-"))
  code <- stringr::str_remove(toupper(code), "MT3D-")
  if (!all(code %in% codes)) {
    rui::alert("Installing codes other than MT3D-USGS or MT3DMS",
               "is currently not supported.")
    rui::stop("Issue with code name.")
  }
  rmti_install_code(code, overwrite = overwrite)
  invisible()
}

#' Install codes
#'
#' @inheritParams rmt_install
rmti_install_code <- function(code, overwrite) {
  os <- Sys.info()['sysname']
  path <- getOption("RMT3DMS.path")
  if (any(grepl("USGS", code)))
    rmti_download_code("MT3D-USGS", path, os, overwrite)
  if (any(grepl("MT3DMS", code, ignore.case = TRUE))) 
    rmti_download_code("MT3DMS", path, os, overwrite)
  invisible()
}

#' Download a code
#'
#' @inheritParams rmt_install
#' @param dir Installation directory.
#' @param os Operating system.
rmti_download_code <- function(code, dir, os, overwrite) {
  # set url
  if(code == "MT3D-USGS") {
    if(os == 'Windows') {
      x <- "https://water.usgs.gov/water-resources/software/MT3D-USGS/mt3dusgs1.1.0.zip"
    } else {
      rui::stop("{code} is not available for your operating system.")
    }
    folder <- gsub('\\.zip', '', basename(x))
  } else if(code == 'MT3DMS') {
    if(os == 'Windows') {
      x <- "https://hydro.geo.ua.edu/mt3d/mt3dms_530.exe"
    } else {
      rui::stop("{code} is not available for your operating system.")
    }
    folder <- gsub('\\.exe', '', basename(x))
  }
  mt_dir <- file.path(dir, code)
  
  # install, if already installed ask what to do
  if(dir.exists(mt_dir)) {
    if(is.null(overwrite) & interactive()) {
      rui::alert("You have already installed {code} in {mt_dir}")
      install <- rui::ask("Do you want to reinstall?")
    } else if (is.null(overwrite)) {
      rui::stop(c("{code} version already exists in {mt_dir}.",
                  "Set overwrite to TRUE if you want replace it."))
    } else if (overwrite) {
      install <- TRUE
    } else {
      install <- FALSE
    }
  } else {
    install <- TRUE
  }
  if(install) {
    if(dir.exists(mt_dir)) unlink(mt_dir, recursive = TRUE, force = TRUE)
    rui::begin("Downloading {code}")
    if(code == 'MT3DMS') {
      temp <- tempdir()
      download.file(x, file.path(temp, basename(x)), quiet = TRUE, mode = 'wb')
    } else {
      temp <- tempfile()
      download.file(x, temp, quiet = TRUE)
    }
    rui::update("Installing {code}")
    if(code == 'MT3DMS') {
      # MT3DMS has a self-extracting file that can be called from a terminal instead of a zip file
      out <- processx::run(file.path(temp, basename(x)), c('/auto', mt_dir), wd = temp, stdout_line_callback = NULL)
      unlink(temp, force = TRUE)
    } else {
      unzip(temp, exdir = dir)
      unlink(temp, force = TRUE)
      fs::file_move(file.path(dir, folder), mt_dir)
    }
    rui::succeed()
    rui::inform("You can find {code} at: {.path {mt_dir}}")
  } else {
    rui::disapprove("Aborting install of {code}")
  }
  invisible()
}