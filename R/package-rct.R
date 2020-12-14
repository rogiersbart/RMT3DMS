
#' Create an \code{RMT3DMS} rct object
#'
#' \code{rmt_create_rct} creates an \code{RMT3DMS} rct object.
#'
#' @param btn \code{RMT3DMS} btn object
#' @param isothm integer flag to set type of sorption (or dual-domain mass transfer). See MT3DMS/MT3D-USGS manual. Defaults to 0.
#' @param ireact integer flag indicating which type of kinetic rate reaction is simulated. See MT3DMS/MT3D-USGS manual. Defaults to 0.
#' @param irctop integer flag indicating how reaction variables are entered. See MT3DMS/MT3D-USGS manual. Defaults to 2 (as 3D-arrays).
#' @param igetsc integer flag specifying if initial concentration of nonequilibrium sorbed or immobile phases should be read. See MT3DMS/MT3D-USGS manual. Defaults to 0 (not read).
#' @param ireaction MT3D-USGS only. Integer flag specifying a reaction module. See MT3D-USGS manual. Defaults to 0 (no reaction module.
#' @param rhob 3d array with bulk density of aquifer medium. Defaults to 157 for all cells.
#' @param prsity2 3d array with porosity values for the immobile domain. Defaults to 0.1 for all cells.
#' @param srconc list with \code{ncomp} 3d arrays specifying the initial sorbed concentrations if \code{igetsc > 0}. Defaults to 0 for all cells.
#' @param sp1 list with \code{ncomp} 3d arrays specifying the first sorption parameter. See MT3DMS/MT3D-USGS manual. Defaults to 0.
#' @param sp1im list with \code{ncomp} 3d arrays speciyfing the immobile domain distribution coefficient for sorption. See MT3DMS/MT3D-USGS manual. Defaults to 0.
#' @param sp2 list with \code{ncomp} 3d arrays specifying the second sorption or dual-domain model parameter. See MT3DMS/MT3D-USGS manual. Defaults to 0.
#' @param rc1 list with \code{ncomp} 3d arrays specifying the primary kinetic parameter. See MT3DMS/MT3D-USGS manual. Defaults to 0.
#' @param rc2 list with \code{ncomp} 3d arrays specifying the secondary kinetic parameter.See MT3DMS/MT3D-USGS manual. Defaults to 0.
#' @param rc3 list with \code{ncomp} 3d arrays specifying the half-saturation constants when \code{ireact = 2}. Defaults to 0.
#' @param yld numeric vector of length \code{ncomp - 1} with the yield coefficients between species when \code{ireact = 3}. \code{ncomp} should be larger than 1 when this feature is used. See MT3DMS/MT3D-USGS manual. 
#' @param ied integer species number representing the electron donor in a EA/ED reaction (\code{ireaction = 1}).
#' @param iea integer species number representing the electron acceptor in a EA/ED reaction (\code{ireaction = 1}).
#' @param f stoiciometric ratio in the simulated equation \code{ED + F*EA -> Product} (\code{ireaction = 1}).
#' @param rec_filename character, name of the input file that will provide parameter information when \code{ireaction = 2}. Defaults to \code{'reaction_module.dat'}.
#' @param ned number of electron donors when \code{ireaction = 2}.
#' @param nea number of electron acceptors when \code{ireaction = 2}.
#' @param nspecial number of special cases when \code{ireaction = 2}.
#' @param ifesld integer flag specifying if solid phase Fe(III) is simulated. Defaults to 0 (not simulated). 
#' @param ispec numeric vector of length \code{nspecial} specifying the sequential order of the species treated as special case.
#' @param specialispec character vector of length \code{nspecial} specifying the keyword of the species treated as special case. See MT3D-USGS manual.
#' @param efcmax numeric vector of length \code{nspecial} specifying the maximum express field capacity (EFC) of the species treated as special case. See MT3D-USGS manual.
#' @param hsc numeric vector of length \code{nea} with the half-saturation constants.
#' @param ic numeric vector of length \code{nea} with the inhibition constants.
#' @param decayrate matrix with \code{nea} rows and \code{ned} columns specifying the decay rates of each EA corresponding to each ED.
#' @param yieldc matrix with \code{nea + ned} rows and \code{ned} columns specifying the yield coefficient of each component corresponding to each ED.
#'
#' @details If \code{irctop < 2}, all 3D variables should be given as 1D vectors with one value per layer.
#'
#' @return an object of class \code{rct}
#' @export
#' @seealso \code{\link{rmt_read_rct}}, \code{\link{rmt_write_rct}}
#' 
#' @examples
rmt_create_rct <- function(btn,
                           isothm = 0,
                           ireact = 0,
                           irctop = 2,
                           igetsc = 0,
                           ireaction = 0,
                           rhob = 157,
                           prsity2 = 0.1,
                           srconc = as.list(rep(0, btn$ncomp)),
                           sp1 = as.list(rep(0, btn$ncomp)),
                           sp1im = as.list(rep(0, btn$ncomp)),
                           sp2 = as.list(rep(0, btn$ncomp)),
                           rc1 = as.list(rep(0, btn$ncomp)),
                           rc2 = as.list(rep(0, btn$ncomp)),
                           rc3 = as.list(rep(0, btn$ncomp)),
                           yld = rep(1, btn$ncomp - 1),
                           ied = 1,
                           iea = 2,
                           f = 1,
                           rec_filename = 'reaction_module.dat',
                           ned = 1,
                           nea = 5,
                           nspecial = 0,
                           ifesld = 0,
                           ispec = rep(0, nspecial),
                           specialispec = rep('SOLID', nspecial),
                           efcmax = rep(0, nspecial),
                           hsc = rep(0.5, nea),
                           ic = rep(0.01, nea),
                           decayrate = matrix(0.0186, nrow = nea, ncol = ned),
                           yieldc = matrix(1, nrow = nea + ned, ncol = ned)
                           ) {
  
  rct <- list()
  
  # Data set 1
  rct$isothm <- isothm
  rct$ireact <- ireact
  rct$irctop <- irctop 
  rct$igetsc <- igetsc
  rct$ireaction <- ireaction
  
  # Data set 2a
  if(rct$isothm %in% c(1,2,3,4,6,-6) || rct$ireaction == 2) {
    if(rct$irctp < 2) {
      rct$rhob <- rmti_ifelse0(length(rhob) == 1, rep(rhob, btn$nlay), rhob)
    } else {
      rct$rhob <- rmt_create_array(rhob, dim = c(btn$nrow, btn$ncol, btn$nlay))
    }
  }
  
  # Data set 2b
  if(rct$isothm %in% c(5,6,-6)) {
    if(rct$irctp < 2) {
      rct$prsity2 <- rmti_ifelse0(length(prsity2) == 1, rep(prsity2, btn$nlay), prsity2)
    } else {
      rct$prsity2 <- rmt_create_array(prsity2, dim = c(btn$nrow, btn$ncol, btn$nlay))
    }
  }
  
  # Data set 2c
  if(rct$igetsc > 0) {
    if(rct$irctop < 2) {
      if(!is.list(srconc) && btn$ncomp == 1) {
        rct$srconc <- list(rmti_ifelse0(length(srconc) == 1, rep(srconc, btn$nlay), srconc))
      } else {
        if(!is.list(srconc) && btn$ncomp > 1) stop('srconc should be a list with ncomp 1D vectors', call. = FALSE)
        rct$srconc <- lapply(seq_len(btn$ncomp), function(i) rmti_ifelse0(length(srconc[[i]]) == 1, rep(srconc[[i]], btn$nlay), srconc[[i]]))
      }
    } else {
      if(!is.list(srconc) && btn$ncomp == 1) {
        rct$srconc <- list(rmt_create_array(srconc, dim = c(btn$nrow, btn$ncol, btn$nlay)))
      } else {
        if(!is.list(srconc) && btn$ncomp > 1) stop('srconc should be a list with ncomp 3D arrays', call. = FALSE)
        rct$srconc <- lapply(seq_len(btn$ncomp), function(i) rmt_create_array(srconc[[i]], dim = c(btn$nrow, btn$ncol, btn$nlay)))
      }
    }
  }
  
  # Data set 3a
  if(rct$isothm != 0) {
    if(rct$irctop < 2) {
      if(!is.list(sp1) && btn$ncomp == 1) {
        rct$sp1 <- list(rmti_ifelse0(length(sp1) == 1, rep(sp1, btn$nlay), sp1))
      } else {
        if(!is.list(sp1) && btn$ncomp > 1) stop('sp1 should be a list with ncomp 1D vectors', call. = FALSE)
        rct$sp1 <- lapply(seq_len(btn$ncomp), function(i) rmti_ifelse0(length(sp1[[i]]) == 1, rep(sp1[[i]], btn$nlay), sp1[[i]]))
      }
    } else {
      if(!is.list(sp1) && btn$ncomp == 1) {
        rct$sp1 <- list(rmt_create_array(sp1, dim = c(btn$nrow, btn$ncol, btn$nlay)))
      } else {
        if(!is.list(sp1) && btn$ncomp > 1) stop('sp1 should be a list with ncomp 3D arrays', call. = FALSE)
        rct$sp1 <- lapply(seq_len(btn$ncomp), function(i) rmt_create_array(sp1[[i]], dim = c(btn$nrow, btn$ncol, btn$nlay)))
      }
    }
  }
  
  # Data set 3b
  if(rct$isothm == -6) {
    if(rct$irctop < 2) {
      if(!is.list(sp1im) && btn$ncomp == 1) {
        rct$sp1im <- list(rmti_ifelse0(length(sp1im) == 1, rep(sp1im, btn$nlay), sp1im))
      } else {
        if(!is.list(sp1im) && btn$ncomp > 1) stop('sp1im should be a list with ncomp 1D vectors', call. = FALSE)
        rct$sp1im <- lapply(seq_len(btn$ncomp), function(i) rmti_ifelse0(length(sp1im[[i]]) == 1, rep(sp1im[[i]], btn$nlay), sp1im[[i]]))
      }
    } else {
      if(!is.list(sp1im) && btn$ncomp == 1) {
        rct$sp1im <- list(rmt_create_array(sp1im, dim = c(btn$nrow, btn$ncol, btn$nlay)))
      } else {
        if(!is.list(sp1im) && btn$ncomp > 1) stop('sp1im should be a list with ncomp 3D arrays', call. = FALSE)
        rct$sp1im <- lapply(seq_len(btn$ncomp), function(i) rmt_create_array(sp1im[[i]], dim = c(btn$nrow, btn$ncol, btn$nlay)))
      }
    }
  }
  
  # Data set 4
  if(rct$isothm != 0) {
    if(rct$irctop < 2) {
      if(!is.list(sp2) && btn$ncomp == 1) {
        rct$sp2 <- list(rmti_ifelse0(length(sp2) == 1, rep(sp2, btn$nlay), sp2))
      } else {
        if(!is.list(sp2) && btn$ncomp > 1) stop('sp2 should be a list with ncomp 1D vectors', call. = FALSE)
        rct$sp2 <- lapply(seq_len(btn$ncomp), function(i) rmti_ifelse0(length(sp2[[i]]) == 1, rep(sp2[[i]], btn$nlay), sp2[[i]]))
      }
    } else {
      if(!is.list(sp2) && btn$ncomp == 1) {
        rct$sp2 <- list(rmt_create_array(sp2, dim = c(btn$nrow, btn$ncol, btn$nlay)))
      } else {
        if(!is.list(sp2) && btn$ncomp > 1) stop('sp2 should be a list with ncomp 3D arrays', call. = FALSE)
        rct$sp2 <- lapply(seq_len(btn$ncomp), function(i) rmt_create_array(sp2[[i]], dim = c(btn$nrow, btn$ncol, btn$nlay)))
      }
    }
  }
  
  # Data set 5
  if(rct$ireact > 0) {
    if(rct$irctop < 2) {
      if(!is.list(rc1) && btn$ncomp == 1) {
        rct$rc1 <- list(rmti_ifelse0(length(rc1) == 1, rep(rc1, btn$nlay), rc1))
      } else {
        if(!is.list(rc1) && btn$ncomp > 1) stop('rc1 should be a list with ncomp 1D vectors', call. = FALSE)
        rct$rc1 <- lapply(seq_len(btn$ncomp), function(i) rmti_ifelse0(length(rc1[[i]]) == 1, rep(rc1[[i]], btn$nlay), rc1[[i]]))
      }
    } else {
      if(!is.list(rc1) && btn$ncomp == 1) {
        rct$rc1 <- list(rmt_create_array(rc1, dim = c(btn$nrow, btn$ncol, btn$nlay)))
      } else {
        if(!is.list(rc1) && btn$ncomp > 1) stop('rc1 should be a list with ncomp 3D arrays', call. = FALSE)
        rct$rc1 <- lapply(seq_len(btn$ncomp), function(i) rmt_create_array(rc1[[i]], dim = c(btn$nrow, btn$ncol, btn$nlay)))
      }
    }
  }
  
  # Data set 6
  if(rct$ireact > 0) {
    if(rct$irctop < 2) {
      if(!is.list(rc2) && btn$ncomp == 1) {
        rct$rc2 <- list(rmti_ifelse0(length(rc2) == 1, rep(rc2, btn$nlay), rc2))
      } else {
        if(!is.list(rc2) && btn$ncomp > 1) stop('rc2 should be a list with ncomp 1D vectors', call. = FALSE)
        rct$rc2 <- lapply(seq_len(btn$ncomp), function(i) rmti_ifelse0(length(rc2[[i]]) == 1, rep(rc2[[i]], btn$nlay), rc2[[i]]))
      }
    } else {
      if(!is.list(rc2) && btn$ncomp == 1) {
        rct$rc2 <- list(rmt_create_array(rc2, dim = c(btn$nrow, btn$ncol, btn$nlay)))
      } else {
        if(!is.list(rc2) && btn$ncomp > 1) stop('rc2 should be a list with ncomp 3D arrays', call. = FALSE)
        rct$rc2 <- lapply(seq_len(btn$ncomp), function(i) rmt_create_array(rc2[[i]], dim = c(btn$nrow, btn$ncol, btn$nlay)))
      }
    }
  }
  
  # Data set 7
  if(rct$ireact == 2) {
    if(rct$irctop < 2) {
      if(!is.list(rc3) && btn$ncomp == 1) {
        rct$rc3 <- list(rmti_ifelse0(length(rc3) == 1, rep(rc3, btn$nlay), rc3))
      } else {
        if(!is.list(rc3) && btn$ncomp > 1) stop('rc3 should be a list with ncomp 1D vectors', call. = FALSE)
        rct$rc3 <- lapply(seq_len(btn$ncomp), function(i) rmti_ifelse0(length(rc3[[i]]) == 1, rep(rc3[[i]], btn$nlay), rc3[[i]]))
      }
    } else {
      if(!is.list(rc3) && btn$ncomp == 1) {
        rct$rc3 <- list(rmt_create_array(rc3, dim = c(btn$nrow, btn$ncol, btn$nlay)))
      } else {
        if(!is.list(rc3) && btn$ncomp > 1) stop('rc3 should be a list with ncomp 3D arrays', call. = FALSE)
        rct$rc3 <- lapply(seq_len(btn$ncomp), function(i) rmt_create_array(rc3[[i]], dim = c(btn$nrow, btn$ncol, btn$nlay)))
      }
    }
  }
  
  # Data set 8
  if(rct$ireact == 3) {
    if(btn$ncomp == 1) stop('When ireact = 3, ncomp should be > 1', call. = FALSE)
    rct$yld <- yld
  }  
  
  # Data set 9a
  if(rct$ireaction == 1) {
    if(btn$ncomp == 1) stop('When ireaction > 0, ncomp should be > 1', call. = FALSE)
    rct$ied <- ied
    rct$iea <- iea
    rct$f <- f
  }
  
  # Data set 9b
  if(rct$ireaction == 2) {
    if(btn$ncomp == 1) stop('When ireaction > 0, ncomp should be > 1', call. = FALSE)
    rct$rec_filename <- rec_filename
    
    # reaction module ----
    # Data set 1
    rct$ned <- ned
    rct$nea <- nea
    rct$nspecial <- nspecial
    rct$ifesld <- ifesld
    
    # Data set 2
    if(rct$nspecial > 0) {
      rct$ispec <- ispec
      rct$specialispec <- specialispec
      rct$efcmax <- efcmax
    }
    
    # Data set 3
    rct$hsc <- rmti_ifelse0(length(hsc) == 1, rep(hsc, rct$nea), hsc)
    rct$ic <- rmti_ifelse0(length(ic) == 1, rep(ic, rct$nea), ic)
    
    # Data set 4
    rct$decayrate <- matrix(decayrate, nrow = rct$nea, ncol = rct$ned)
    
    # Data set 5
    rct$yieldc <- matrix(yieldc, nrow = rct$nea + rct$ned, ncol = rct$ned)
  }
  
  class(rct) <- c('rct','rmt_package')
  return(rct)
}


#' Read an MT3DMS Chemical Reaction Package file
#' 
#' \code{rmt_read_rct} reads in an MT3DMS/MT3D-USGS reaction package file and returns it as an \code{\link{RMT3DMS}} rct object.
#' 
#' @param file filename; typically '*.rct'
#' @param btn \code{RMT3DMS} btn object
#' @param ... optional arguments passed to \code{\link{rmti_parse_array}}
#' @return object of class \code{rct}
#' @export
rmt_read_rct <- function(file = {cat('Please select rct file ...\n'); file.choose()},
                         btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())},
                         ...) {
  
  rct_lines <- readr::read_lines(file)
  rct <- list()
  
  # Data set 1
  data_set_1 <- rmti_parse_variables(rct_lines, n = 5)
  rct$isothm <- as.numeric(data_set_1$variables[1])
  rct$ireact <- as.numeric(data_set_1$variables[2])
  rct$irctop <- as.numeric(data_set_1$variables[3])
  rct$igetsc <- as.numeric(data_set_1$variables[4])
  rct$ireaction <- as.numeric(data_set_1$variables[5])
  if(is.na(rct$ireaction)) rct$ireaction <- 0
  rct_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # Data set 2A
  if(rct$isothm %in% c(1,2,3,4,6,-6) || rct$ireaction == 2) {
    if(rct$irctop < 2) {
      data_set_2a <- rmti_parse_array(rct_lines, nrow = 1, ncol = btn$nlay, nlay = 1, ndim = 1, file = file, ...)
    } else {
      data_set_2a <- rmti_parse_array(rct_lines, nrow = btn$nrow, ncol = btn$ncol, nlay = btn$nlay, ndim = 3, file = file, ...)
    }
    rct$rhob <- data_set_2a$array
    rct_lines <- data_set_2a$remaining_lines
    rm(data_set_2a)
  }
  
  # Data set 2B
  if(rct$isothm %in% c(5,6,-6)) {
    if(rct$irctop < 2) {
      data_set_2b <- rmti_parse_array(rct_lines, nrow = 1, ncol = btn$nlay, nlay = 1, ndim = 1, file = file, ...)
    } else {
      data_set_2b <- rmti_parse_array(rct_lines, nrow = btn$nrow, ncol = btn$ncol, nlay = btn$nlay, ndim = 3, file = file, ...)
    }
    rct$prsity2 <- data_set_2b$array
    rct_lines <- data_set_2b$remaining_lines
    rm(data_set_2b)
  }
  
  # Data set 2C
  if(rct$igetsc > 0) {
    rct$srconc <- list()
    for(species in 1:btn$ncomp) {
      if(rct$irctop < 2) {
        data_set_2c <- rmti_parse_array(rct_lines, nrow = 1, ncol = btn$nlay, nlay = 1, ndim = 1, file = file, ...)
      } else {
        data_set_2c <- rmti_parse_array(rct_lines, nrow = btn$nrow, ncol = btn$ncol, nlay = btn$nlay, ndim = 3, file = file, ...)
      }
      rct$srconc[[species]] <- data_set_2c$array
      rct_lines <- data_set_2c$remaining_lines
      rm(data_set_2c)
    }
  }
  
  # Data set 3a
  if(rct$isothm != 0) {
    rct$sp1 <- list()
    for(species in 1:btn$ncomp) {
      if(rct$irctop < 2) {
        data_set_3a <- rmti_parse_array(rct_lines, nrow = 1, ncol = btn$nlay, nlay = 1, ndim = 1, file = file, ...)
      } else {
        data_set_3a <- rmti_parse_array(rct_lines, nrow = btn$nrow, ncol = btn$ncol, nlay = btn$nlay, ndim = 3, file = file, ...)
      }
      rct$sp1[[species]] <- data_set_3a$array
      rct_lines <- data_set_3a$remaining_lines
      rm(data_set_3a)
    }
  }
  
  # Data set 3b
  if(rct$isothm == -6) {
    rct$sp1im <- list()
    for(species in 1:btn$ncomp) {
      if(rct$irctop < 2) {
        data_set_3b <- rmti_parse_array(rct_lines, nrow = 1, ncol = btn$nlay, nlay = 1, ndim = 1, file = file, ...)
      } else {
        data_set_3b <- rmti_parse_array(rct_lines, nrow = btn$nrow, ncol = btn$ncol, nlay = btn$nlay, ndim = 3, file = file, ...)
      }
      rct$sp1im[[species]] <- data_set_3b$array
      rct_lines <- data_set_3b$remaining_lines
      rm(data_set_3b)
    }
  }
  
  # Data set 4
  if(rct$isothm != 0) {
    rct$sp2 <- list()
    for(species in 1:btn$ncomp) {
      if(rct$irctop < 2) {
        data_set_4 <- rmti_parse_array(rct_lines, nrow = 1, ncol = btn$nlay, nlay = 1, ndim = 1, file = file, ...)
      } else {
        data_set_4 <- rmti_parse_array(rct_lines, nrow = btn$nrow, ncol = btn$ncol, nlay = btn$nlay, ndim = 3, file = file, ...)
      }
      rct$sp2[[species]] <- data_set_4$array
      rct_lines <- data_set_4$remaining_lines
      rm(data_set_4)
    }
  }
  
  # Data set 5
  if(rct$ireact > 0) {
    rct$rc1 <- list()
    for(species in 1:btn$ncomp) {
      if(rct$irctop < 2) {
        data_set_5 <- rmti_parse_array(rct_lines, nrow = 1, ncol = btn$nlay, nlay = 1, ndim = 1, file = file, ...)
      } else {
        data_set_5 <- rmti_parse_array(rct_lines, nrow = btn$nrow, ncol = btn$ncol, nlay = btn$nlay, ndim = 3, file = file, ...)
      }
      rct$rc1[[species]] <- data_set_5$array
      rct_lines <- data_set_5$remaining_lines
      rm(data_set_5)
    }
  }
  
  # Data set 6
  if(rct$ireact > 0) {
    rct$rc2 <- list()
    for(species in 1:btn$ncomp) {
      if(rct$irctop < 2) {
        data_set_6 <- rmti_parse_array(rct_lines, nrow = 1, ncol = btn$nlay, nlay = 1, ndim = 1, file = file, ...)
      } else {
        data_set_6 <- rmti_parse_array(rct_lines, nrow = btn$nrow, ncol = btn$ncol, nlay = btn$nlay, ndim = 3, file = file, ...)
      }
      rct$rc2[[species]] <- data_set_6$array
      rct_lines <- data_set_6$remaining_lines
      rm(data_set_6)
    }
  }
  
  # Data set 7
  if(rct$ireact == 2) {
    rct$rc3 <- list()
    for(species in 1:btn$ncomp) {
      if(rct$irctop < 2) {
        data_set_7 <- rmti_parse_array(rct_lines, nrow = 1, ncol = btn$nlay, nlay = 1, ndim = 1, file = file, ...)
      } else {
        data_set_7 <- rmti_parse_array(rct_lines, nrow = btn$nrow, ncol = btn$ncol, nlay = btn$nlay, ndim = 3, file = file, ...)
      }
      rct$rc3[[species]] <- data_set_7$array
      rct_lines <- data_set_7$remaining_lines
      rm(data_set_7)
    }
  }
  
  # Data set 8
  if(rct$ireact == 3) {
    rct$yld <- vector(mode = 'numeric', length = btn$ncomp - 1)
    for(species in 1:(btn$ncomp - 1)) {
      data_set_8 <- rmti_parse_variables(rct_lines, n = 1)
      rct$rc3[species] <- as.numeric(data_set_8$variables[1])
      rct_lines <- data_set_8$remaining_lines
      rm(data_set_8)
    }
  }
  
  # Data set 9a
  if(rct$ireaction == 1) {
    data_set_9a <- rmti_parse_variables(rct_lines, n = 3)
    rct$ied <- as.numeric(data_set_9a$variables[1])
    rct$iea <- as.numeric(data_set_9a$variables[2])
    rct$f <- as.numeric(data_set_9a$variables[3])
    rct_lines <- data_set_9a$remaining_lines
    rm(data_set_9a)
  }
  
  # Data set 9b
  if(rct$ireaction == 2) {
    data_set_9b <- rmti_parse_variables(rct_lines, n = 1, character = TRUE, width = 500)
    rct$rec_filename <- as.character(data_set_9b$variables[1])
    rct_lines <- data_set_9b$remaining_lines
    rm(data_set_9b)
    
    rct_file <- file.path(dirname(file), rct$rec_filename)
    rec_lines <- readr::read_lines(rct_file)
    
    # read rec_file ----
    
    # Data set 0
    data_set_0 <- rmti_parse_comments(rec_lines)
    comment(rct) <- data_set_0$comments
    rec_lines <- data_set_0$remaining_lines
    rm(data_set_0)
    
    # Data set 1
    data_set_1 <- rmti_parse_variables(rec_lines, n = 4, format = 'free')
    rct$ned <- as.numeric(data_set_1$variables[1])
    rct$nea <- as.numeric(data_set_1$variables[2])
    rct$nspecial <- as.numeric(data_set_1$variables[3])
    rct$ifesld <- as.numeric(data_set_1$variables[4])
    rec_lines <- data_set_1$remaining_lines
    rm(data_set_1)
    
    # Data set 2
    if(rct$nspecial > 0) {
      rct$ispec <- rct$specialispec <- rct$efcmax <- vector(mode = 'numeric', length = rct$nspecial)
      for(i in 1:rct$nspecial) {
        data_set_2 <- rmti_parse_variables(rec_lines, n = 3, format = 'free')
        rct$ispec[i] <- as.numeric(data_set_2$variables[1])
        rct$specialispec[i] <- as.character(data_set_2$variables[2])
        rct$efcmax[i] <- as.numeric(data_set_2$variables[3])
        rec_lines <- data_set_2$remaining_lines
        rm(data_set_2)
      }
    }
    
    # Data set 3
    rct$hsc <- rct$ic <- vector(mode = 'numeric', length = rct$nea)
    for(i in 1:rct$nea) {
      data_set_3 <- rmti_parse_variables(rec_lines, n = 2, format = 'free')
      rct$hsc[i] <- as.numeric(data_set_3$variables[1])
      rct$ic[i] <- as.numeric(data_set_3$variables[2])
      rec_lines <- data_set_3$remaining_lines
      rm(data_set_3)
    }
    
    # Data set 4
    rct$decayrate <- matrix(NA, nrow = rct$nea, ncol = rct$ned)
    for(i in 1:rct$nea) {
      data_set_4 <- rmti_parse_variables(rec_lines, n = rct$ned, format = 'free')
      rct$decayrate[i,] <- as.numeric(data_set_4$variables[1:rct$ned])
      rec_lines <- data_set_4$remaining_lines
      rm(data_set_4)
    }
    
    # Data set 5
    rct$yieldc <- matrix(NA, nrow = rct$nea + rct$ned, ncol = rct$ned)
    for(i in 1:(rct$nea + rct$ned)) {
      data_set_5 <- rmti_parse_variables(rec_lines, n = rct$ned, format = 'free')
      rct$yieldc[i,] <- as.numeric(data_set_5$variables[1:rct$ned])
      rec_lines <- data_set_5$remaining_lines
      rm(data_set_5)
    }
  }
  
  class(rct) <- c('rct','rmt_package')
  return(rct)
}

#' Write an MT3DMS Chemical Reaction Package file
#' 
#' @param rct an \code{RMT3DMS} rct object
#' @param file filename to write to; typically '*.rct'
#' @param btn an \code{RMT3DMS} btn object
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmti_write_array}. 
#' @return \code{NULL}
#' @export
rmt_write_rct <- function(rct,
                          file = {cat('Please select rct file to overwrite or provide new filename ...\n'); file.choose()},
                          btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())},
                          iprn=-1,
                          ...) {
  
  mf_style <- btn$modflowstylearrays
  
  # Data set 1
  rmti_write_variables(rct$isothm, rct$ireact, rct$irctop, rct$igetsc, rct$ireaction, file = file, integer = TRUE, append = FALSE)
  
  # Data set 2a
  if(rct$isothm %in% c(1,2,3,4,6,-6) || rct$ireaction == 2) {
    rmti_write_array(rct$rhob, file = file, iprn = iprn, mf_style = mf_style, ...)
  }
  
  # Data set 2b
  if(rct$isothm %in% c(5,6,-6)) {
    rmti_write_array(rct$prsity2, file = file, iprn = iprn, mf_style = mf_style, ...)
  }
  
  # Data set 2c
  if(rct$igetsc > 0) {
    for(species in 1:btn$ncomp) {
      rmti_write_array(rct$srconc[[species]], file = file, iprn = iprn, mf_style = mf_style, ...)
    }
  }
  
  # Data set 3a
  if(rct$isothm != 0) {
    for(species in 1:btn$ncomp) {
      rmti_write_array(rct$sp1[[species]], file = file, iprn = iprn, mf_style = mf_style, ...)
    }
  }
  
  # Data set 3b
  if(rct$isothm == -6) {
    for(species in 1:btn$ncomp) {
      rmti_write_array(rct$sp1im[[species]], file = file, iprn = iprn, mf_style = mf_style, ...)
    }
  }
  
  # Data set 4
  if(rct$isothm != 0) {
    for(species in 1:btn$ncomp) {
      rmti_write_array(rct$sp2[[species]], file = file, iprn = iprn, mf_style = mf_style, ...)
    }
  }
  
  # Data set 5
  if(rct$ireact > 0) {
    for(species in 1:btn$ncomp) {
      rmti_write_array(rct$rc1[[species]], file = file, iprn = iprn, mf_style = mf_style, ...)
    }
  }
  
  # Data set 6
  if(rct$ireact > 0) {
    for(species in 1:btn$ncomp) {
      rmti_write_array(rct$rc2[[species]], file = file, iprn = iprn, mf_style = mf_style, ...)
    }
  }
  
  # Data set 7
  if(rct$ireact == 2) {
    for(species in 1:btn$ncomp) {
      rmti_write_array(rct$rc3[[species]], file = file, iprn = iprn, mf_style = mf_style, ...)
    }
  }
  
  # Data set 8
  if(rct$ireact == 3) {
    for(i in 1:(btn$ncomp - 1)) {
      rmti_write_variables(rct$yld[i], file = file)
    }
  }
  
  # Data set 9a
  if(rct$ireaction == 1) {
    rmti_write_variables(as.integer(rct$ied), as.integer(rct$iea), rct$f, file = file)
  }
  
  # Data set 9b
  if(rct$ireaction == 2) {
    rmti_write_variables(rct$rec_filename, file = file, format = 'free')
    
    # write rec_file ----
    rec_file <- file.path(dirname(file), rct$rec_filename)
    
    # Data set 0
    v <- packageDescription("RMT3DMS")$Version
    cat(paste('# MT3D-USGS Reaction Module File created by RMT3DMS, version',v,'\n'), file=rec_file)
    cat(paste('#', comment(rct)), '\n', file=rec_file, append=TRUE)
    
    # Data set 1
    rmti_write_variables(rct$ned, rct$nea, rct$nspecial, rct$ifesld, file = rec_file, format = 'free')
    
    # Data set 2
    if(rct$nspecial > 0) {
      for(i in 1:rct$nspecial) {
        rmti_write_variables(rct$ispec[i], rct$specialispec[i], rct$efcmax[i], file = rec_file, format = 'free')
      }
    }
    
    # Data set 3
    for(i in 1:rct$nea) {
      rmti_write_variables(rct$hsc[i], rct$ic[i], file = rec_file, format = 'free')
    }
    
    # Data set 4
    for(i in 1:rct$nea) {
      rmti_write_variables(c(rct$decayrate[i,]), file = rec_file, format = 'free')
    }
    
    # Data set 5
    for(i in 1:(rct$nea + rct$ned)) {
      rmti_write_variables(c(rct$yieldc[i,]), file = rec_file, format = 'free')
    }
  }
  
}
