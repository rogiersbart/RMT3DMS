
#' Create an \code{RMT3DMS} mt3dms object
#'
#' \code{rmt_create} creates an \code{RMT3DMS} mt3dms object from \code{rmt_package} objects 
#'
#' @param ... (list of) \code{RMT3DMS} objects of class \code{rmt_package} to be included in the mt3dms object. If a \code{nam} object is not provided, it is added automatically.
#' @param recreate_nam logical; if a \code{nam} object is supplied, should it be recreated from scratch ? Defaults to FALSE
#' @param ftl path to the flow-transport link file in the \code{nam} object; typically '.ftl'. Only used when a \code{nam} object is not supplied or when \code{recreate_nam = TRUE}.
#' @param ftl_free logical; is the flow-transport link file in the \code{nam} object written in free (formatted) format (TRUE) or binary (unformatted) (FALSE)? if NULL (default), it is guessed from reading \code{ftl}. Only used when a \code{nam} object is not supplied or when \code{recreate_nam = TRUE}.
#' @param basename character specifying the basename of the files if the nam object is (re)created. The default (\code{NULL}) sets input basenames to 'input' and output to 'output'.
#' @param reset_timprs logical, when a \code{tob} object is present, should the observed times be added to \code{btn$timprs} in order to write output at correct times? Defaults to FALSE. Note that this also resets \code{btn$nprs}
#' @return a \code{mt3dms} object which is a list containing all MT3DMS/MT3D-USGS packages
#' @export
#' @seealso \code{\link{rmt_read}}, \code{\link{rmt_write}}
#' @examples 
#' btn <- rmt_create_btn()
#' adv <- rmt_create_adv()
#' gcg <- rmt_create_gcg()
#' nam <- rmt_create_nam(btn, adv, gcg, ftl = 'output.ftl', ftl_free = TRUE)
#' 
#' rmt_create(btn, adv, gcg, nam)
#' rmt_create(btn, adv, gcg, nam, recreate_nam = TRUE, basename = 'ex1')
#' rmt_create(btn, adv, gcg, ftl = 'output.ftl', ftl_free = FALSE)
rmt_create <- function(..., recreate_nam = FALSE, ftl, ftl_free = NULL, print = FALSE, basename = NULL, reset_timprs = FALSE) {
  
  mt3dms <- list(...)
  if(length(mt3dms) == 1 && inherits(mt3dms[[1]], c('list', 'mt3dms')) && !('rmt_package' %in% class(mt3dms[[1]]))) mt3dms <- unclass(mt3dms[[1]])
  # check if all input are rmt_packages & add all input objects
  all_rmt <- vapply(mt3dms, function(i) 'rmt_package' %in% class(i), TRUE)
  if(prod(all_rmt) == 0) stop('Please make sure all objects are RMT3DMS rmt_package objects representing MT3DMS input', call. = FALSE)
  
  ftype <- vapply(mt3dms, function(i) class(i)[which(class(i) == 'rmt_package') - 1], 'text')
  names(mt3dms) <- ftype

  # find nam object; if not present or recreate_nam = TRUE, create one. If present, check if all packages are also in nam
  if(!('mt3d_nam' %in% ftype)) {
    mt3dms$nam <- rmt_create_nam(mt3dms, ftl = ftl, ftl_free = ftl_free, basename = basename, print = print)
  } else if(recreate_nam) {
    nam_old <- mt3dms[[which(names(mt3dms) == 'mt3d_nam')]]
    ftl <- nam_old$fname[which(nam_old$ftype %in% c('FTL', 'FT6'))]
    ftl_free <- grepl('FREE', nam_old$options[which(nam_old$ftype == 'FTL')], ignore.case = TRUE)
    print <- grepl('PRINT', nam_old$options[which(nam_old$ftype == 'FTL')], ignore.case = TRUE)
    mt3dms$nam <- rmt_create_nam(mt3dms[-which(names(mt3dms) == 'mt3d_nam')], ftl = ftl, ftl_free = ftl_free, basename = basename, print = print)
    mt3dms$mt3d_nam <- NULL
  } else {  
    names(mt3dms)[which(names(mt3dms) == 'mt3d_nam')] <- 'nam'
    df <- rmti_list_packages(type = 'usgs')
    mt_types <- df$ftype[which(df$rmt %in% ftype)]
    nam_types <- mt3dms$nam$ftype[which(!(mt3dms$nam$ftype %in% c('DATA', 'DATA(BINARY)', 'LIST', 'GLOBAL', 'DATAGLO', 'DATAGLO(BINARY)', 'FTL', 'FT6')))]
    if(!isTRUE(all.equal(sort(mt_types), sort(nam_types)))) stop('Please make sure the supplied packages are listed in the nam file.', call. = FALSE)
  }
  
  # check for btn
  if(!('btn' %in% ftype)) stop('A MT3DMS simulation must at least have a btn package', call. = FALSE)
  
  # reset btn$timprs if requested when TOB is present
  if('tob' %in% ftype && reset_timprs) {
    timprs <- btn$timprs
    if(mt3dms$tob$inconcobs > 0) {
      timprs <- append(timprs, mt3dms$tob$concentrations$timeobs)
    }
    if(mt3dms$tob$influxobs > 0) {
      timprs <- append(timprs, mt3dms$tob$fluxobs$time)
    }
    timprs <- sort(unique(timprs))
    timprs <- timprs[which(timprs > 0)]
    nprs <- length(timprs)
    if(nprs != 0) {
      mt3dms$btn$nprs <- nprs
      mt3dms$btn$timprs <- timprs
    }
  }
  
  class(mt3dms) <- c('mt3dms')
  return(mt3dms)
  
}

#' Read a MT3DMS model
#'
#' \code{rmt_read} reads in a MT3DMS/MT3D-USGS model and returns it as a \code{mt3dms} object
#'
#' @param file NAME file; typically '*.mt_nam'
#' @param output logical; should output also be read. Defaults to FALSE.
#' @param precision either \code{'single'} or \code{'double'}. Specifies the precision of binary files. Defaults to \code{'single'}.
#' @param verbose logical; should information on reading files be printed to the console ? Defaults to TRUE.
#' 
#' @note If the MT3DMS model does not contain a NAME file, use the \code{rmt_read_*} functions to read in the packages individually.
#' @return a \code{mt3dms} object which is a list containing all MT3DMS packages and optionally, model output
#' @export
#' @seealso \code{\link{rmt_create}}, \code{\link{rmt_write}}
#' @examples
rmt_read <- function(file = {cat('Please select nam file ...\n'); file.choose()},
                     output = FALSE,
                     precision = 'single',
                     verbose = TRUE) {
  
  mt3dms <- list()
  print_reading <- function(package, file, output = FALSE) {
    cat(paste0("---------------------------------", '\n'))
    if(output) {
      cat(paste0(paste("  Reading", package, "output from file", file), '\n'))
    } else {
      cat(paste0(paste("  Reading", package, "package from file", file), '\n'))
    }
  }
  
  # basic
  # first read nam, then btn
  if(verbose) print_reading('NAM', file = file)
  mt3dms$nam <- rmt_read_nam(file = file)
  dir_nam <- dirname(file)
  fname <- file.path(dir_nam, mt3dms$nam$fname)
  ftype <- mt3dms$nam$ftype
  
  if(verbose) print_reading('BTN', file = fname[which(mt3dms$nam$ftype == 'BTN')])
  mt3dms$btn <- rmt_read_btn(file = fname[which(mt3dms$nam$ftype == 'BTN')], nam = mt3dms$nam, precision = precision)
  ftype <- ftype[-which(ftype == 'BTN')]
  
  # ADV
  if('ADV'%in% ftype) {
    if(verbose) print_reading('ADV', file = fname[which(mt3dms$nam$ftype == 'ADV')])
    mt3dms$adv <- rmt_read_adv(file = fname[which(mt3dms$nam$ftype == 'ADV')])
    ftype <- ftype[-which(ftype == 'ADV')]
  }
  
  # DSP
  if('DSP'%in% ftype) {
    if(verbose) print_reading('DSP', file = fname[which(mt3dms$nam$ftype == 'DSP')])
    mt3dms$dsp <- rmt_read_dsp(file = fname[which(mt3dms$nam$ftype == 'DSP')], btn = mt3dms$btn, nam = mt3dms$nam, precision = precision)
    ftype <- ftype[-which(ftype == 'DSP')]
  }
  
  # SSM
  if('SSM'%in% ftype) {
    if(verbose) print_reading('SSM', file = fname[which(mt3dms$nam$ftype == 'SSM')])
    mt3dms$ssm <- rmt_read_ssm(file = fname[which(mt3dms$nam$ftype == 'SSM')], btn = mt3dms$btn, ftl = fname[which(mt3dms$nam$ftype == 'FTL')],
                               ftl_free =  grepl('FREE', mt3dms$nam$options[which(mt3dms$nam$ftype == 'FTL')], ignore.case = TRUE), nam = mt3dms$nam, precision = precision)
    ftype <- ftype[-which(ftype == 'SSM')]
  }
  
  # GCG
  if('GCG'%in% ftype) {
    if(verbose) print_reading('GCG', file = fname[which(mt3dms$nam$ftype == 'GCG')])
    mt3dms$gcg <- rmt_read_gcg(file = fname[which(mt3dms$nam$ftype == 'GCG')])
    ftype <- ftype[-which(ftype == 'GCG')]
  }
  
  # RCT
  if('RCT'%in% ftype) {
    if(verbose) print_reading('RCT', file = fname[which(mt3dms$nam$ftype == 'RCT')])
    mt3dms$rct <- rmt_read_rct(file = fname[which(mt3dms$nam$ftype == 'RCT')], btn = mt3dms$btn, nam = mt3dms$nam, precision = precision)
    ftype <- ftype[-which(ftype == 'RCT')]
  }
  
  # TOB
  if('TOB'%in% ftype) {
    if(verbose) print_reading('TOB', file = fname[which(mt3dms$nam$ftype == 'TOB')])
    mt3dms$tob <- rmt_read_tob(file = fname[which(mt3dms$nam$ftype == 'TOB')])
    ftype <- ftype[-which(ftype == 'TOB')]
  }
  
  # CTS
  
  # UZT
  
  # HSS
  
  # TSO
  
  # LKT
  
  # SFT
  
  # TODO read output files with non-standard names
  # output
  if(output) {
    
    # TOB output; if PST present, don't need to read OCN & MFX
    if('tob' %in% names(mt3dms)) {
      # TODO support reading PST
      # if(mt3dms$tob$insaveobs > 0) {
      #   flname <- paste0(mt3dms$tob$outnam, '.pst')
      #   fl <- file.path(dir_nam, flname)
      #   if(file.exists(fl)) {
      #     if(verbose) print_reading('Binary Observed Concentration & Mass Flux', file = fl, output = TRUE)
      #     mt3dms$pst <- rmt_read_pst(fl, precision = precision)
      #   }
      # }
      if(mt3dms$tob$inconcobs > 0) {
        flname <- paste0(mt3dms$tob$outnam, '.ocn')
        fl <- file.path(dir_nam, flname)
        if(file.exists(fl)) {
          if(verbose) print_reading('Observed Concentration', file = fl, output = TRUE)
          mt3dms$ocn <- rmt_read_ocn(fl)
        }
      }
      if(mt3dms$tob$influxobs > 0) {
        flname <- paste0(mt3dms$tob$outnam, '.mfx')
        fl <- file.path(dir_nam, flname)
        if(file.exists(fl)) {
          if(verbose) print_reading('Observed Mass Flux', file = fl, output = TRUE)
          mt3dms$mfx <- rmt_read_mfx(fl)
        }
      }
    }
    
    # ucn
    if(mt3dms$btn$savucn) {
      for(icomp in 1:mt3dms$btn$ncomp) {
        flname <- paste0('MT3D', formatC(icomp, width = 3, flag = '0'), '.UCN')
        fl <- file.path(dir_nam, flname)
        if(file.exists(fl)) {
          if(verbose) print_reading(paste('Species', icomp, 'concentration'), file = fl, output = TRUE)
          mt3dms[[paste0('ucn', formatC(icomp, width = 3, flag = '0'))]] <- rmt_read_ucn(fl, btn = mt3dms$btn, solute = icomp, precision = precision)
        }
        
        # Sorbed
        flname <- paste0('MT3D', formatC(icomp, width = 3, flag = '0'), 'S.UCN')
        fl <- file.path(dir_nam, flname)
        if(file.exists(fl)) {
          if(verbose) print_reading(paste('Species', icomp, 'sorbed concentration'), file = fl, output = TRUE)
          mt3dms[[paste0('sucn', formatC(icomp, width = 3, flag = '0'))]] <- rmt_read_ucn(fl, btn = mt3dms$btn, solute = icomp, precision = precision)
        }
      }
    }
    
    # obs
    if(!is.null(mt3dms$btn$obs) && mt3dms$btn$nprobs != 0) {
      for(icomp in 1:mt3dms$btn$ncomp) {
        flname <- paste0('MT3D', formatC(icomp, width = 3, flag = '0'), '.OBS')
        fl <- file.path(dir_nam, flname)
        if(file.exists(fl)) {
          if(verbose) print_reading(paste('Species', icomp, 'concentration observation'), file = fl, output = TRUE)
          mt3dms[[paste0('obs', formatC(icomp, width = 3, flag = '0'))]] <- rmt_read_obs(fl, btn = mt3dms$btn, solute = icomp)
        }
      }
    }
    
    # mas
    if(mt3dms$btn$chkmas && mt3dms$btn$nprmas != 0) {
      for(icomp in 1:mt3dms$btn$ncomp) {
        flname <- paste0('MT3D', formatC(icomp, width = 3, flag = '0'), '.MAS')
        fl <- file.path(dir_nam, flname)
        if(file.exists(fl)) {
          if(verbose) print_reading(paste('Species', icomp, 'mass balance'), file = fl, output = TRUE)
          mt3dms[[paste0('mas', formatC(icomp, width = 3, flag = '0'))]] <- rmt_read_mas(fl)
        }
      }
    }
    
    # budget
    if(verbose) print_reading('Cumulative mass budget', file = fname[which(mt3dms$nam$ftype == "LIST")], output = TRUE)
    mt3dms$bud <- rmt_read_bud(file = fname[which(mt3dms$nam$ftype == "LIST")])
    
  } 
  
  # remove output from ftype
  ftype <- ftype[-which(ftype %in% c('DATA', 'DATA(BINARY)', 'LIST', 'GLOBAL', 'DATAGLO', 'DATAGLO(BINARY)', 'FTL', 'FT6'))]  
  
  # warning for not-supported packages
  if(length(ftype) > 0) warning(paste0('Following packages are not supported yet: \n ', paste(ftype, collapse = '\n ')), call. = FALSE)
  
  class(mt3dms) <- c('mt3dms')
  return(mt3dms)
}

#' Write a MT3DMS model
#'
#' \code{rmt_write} writes all input packages in a \code{RMT3DMS} mt3dms object to a directory
#'
#' @param mt3dms \code{RMT3DMS} mt3dms object
#' @param file filename of the name file to write
#' @param exclude character vector with packages names to exclude from the simulation. Defaults to NULL
#' @param suppress logical; remove non-supported (and thus not written) packages in the NAME file ? Defaults to FALSE
#' @param verbose logical; should information on writing files be printed to the console ? Defaults to TRUE.
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
#' @details All arrays use IREAD 0 or 103 (constant/free) unless \code{mt3dms$btn$modflowstylearrays = TRUE} in which case free-format headers INTERNAL or CONSTANT are used.
#'          All packages will be written according to the filenames (fname) defined in the nam object.
#'          To prevent any files being overwritten, it is best to write to an empty directory.
#' @seealso \code{\link{rmt_create}}, \code{\link{rmt_write}}
#' @examples
#' btn <- rmt_create_btn()
#' adv <- rmt_create_adv()
#' gcg <- rmt_create_gcg()
#' nam <- rmt_create_nam(btn, adv, gcg, ftl = 'output.ftl', ftl_free = TRUE)
#' 
#' m <- rmt_create(btn, adv, gcg, nam)
#' f <- tempfile()
#' 
#' rmt_write(m, f, verbose = FALSE)
#' 
#' f2 <- tempfile()
#' \dontrun{
#' rmt_write(m, f2, exclude = 'dsp')
#' }
rmt_write <- function(mt3dms, 
                      file = {cat('Please select nam file to overwrite or provide new filename ...\n'); file.choose()},
                      exclude = NULL,
                      suppress = FALSE,
                      verbose = TRUE,
                      iprn = -1) {
  
  print_writing <- function(package, file) {
    cat(paste0("---------------------------------", '\n'))
    cat(paste0(paste("  Writing", package, "package to file", file), '\n'))
  }
  
  dir_name <- dirname(file)
  packages <- rmti_list_packages(type = 'usgs')
  
  # exclude packages
  if(!is.null(exclude)) {
    ftype <- packages$ftype[which(packages$rmt %in% exclude)]
    mt3dms$nam <- mt3dms$nam[-which(mt3dms$nam$ftype %in% ftype), ]
  }
  
  # all packages in a RMT3DMS mt3dms object are supported by RMT3DMS
  ftype <- mt3dms$nam$ftype
  ftype <- packages$rmt[packages$ftype %in% ftype]
  
  # not supported
  not_supported <- mt3dms$nam$ftype[-which(mt3dms$nam$ftype %in% c('DATA', 'DATA(BINARY)', 'LIST', 'GLOBAL', 'DATAGLO', 'DATAGLO(BINARY)', 'FTL', 'FT6'))]
  not_supported <- not_supported[-which(not_supported %in% packages$ftype)]
  
  if(length(not_supported) > 0) {
    warning(paste0('Packages in the NAME file not written:  \n ', paste(not_supported, collapse = '\n ')), call. = FALSE)
    if(suppress) {
      warning('Removing non-supported packages in the NAME file', call. = FALSE)
      mt3dms$nam <- mt3dms$nam[-which(mt3dms$nam$ftype %in% not_supported),]
    }
  } 
  
  # first write nam & btn, then everything else.

  # nam
  if(verbose) print_writing('NAME', file = file)
  rmt_write_nam(nam = mt3dms$nam, file = file)
  
  # btn
  if(verbose) print_writing('BTN', file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'BTN')]))
  rmt_write_btn(btn = mt3dms$btn, file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'BTN')]), iprn = iprn)
  
  # adv
  if('adv' %in% ftype) {
    if(verbose) print_writing('ADV', file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'ADV')]))
    rmt_write_adv(adv = mt3dms$adv, file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'ADV')]))
  }

  # dsp
  if('dsp' %in% ftype) {
    if(verbose) print_writing('DSP', file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'DSP')]))
    rmt_write_dsp(dsp = mt3dms$dsp, file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'DSP')]), btn = mt3dms$btn, iprn = iprn)
  }

  # ssm
  if('ssm' %in% ftype) {
    if(verbose) print_writing('SSM', file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'SSM')]))
    rmt_write_ssm(ssm = mt3dms$ssm, file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'SSM')]), btn = mt3dms$btn, iprn = iprn)
  }
  
  # gcg
  if('gcg' %in% ftype) {
    if(verbose) print_writing('GCG', file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'GCG')]))
    rmt_write_gcg(gcg = mt3dms$gcg, file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'GCG')]))
  }

  # rct
  if('rct' %in% ftype) {
    if(verbose) print_writing('RCT', file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'RCT')]))
    rmt_write_rct(rct = mt3dms$rct, file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'RCT')]), btn = mt3dms$btn, iprn = iprn)
  }
  
  # tob
  if('tob' %in% ftype) {
    if(verbose) print_writing('TOB', file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'TOB')]))
    rmt_write_tob(tob = mt3dms$tob, file = file.path(dir_name, mt3dms$nam$fname[which(mt3dms$nam$ftype == 'TOB')]))
  }
  
  # cts
  
  # uzt
  
  # hss
  
  # tso
  
  # lkt
  
  # sft
  
}
