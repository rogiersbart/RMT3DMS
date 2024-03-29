#' Generic function for static 2D plotting
#' 
#' @rdname rmt_plot
#' @export
rmt_plot <- function(...) {
  UseMethod('rmt_plot')
}

#' Plot a MT3DMS mass budget
#' 
#' \code{rmt_plot.cbud} plots a MT3DMS mass budget
#'
#' @param cbud \code{RMT3DMS} cbud object as obtained by \code{rmt_read_bud}
#' @param btn \code{RMT3DMS} btn object
#' @param icomp integer selecting for which species to plot the budget; defaults to 1
#' @param what character; what to plot, "cumulative", "total", "difference" or "discrepancy". Defaults to "cumulative".
#' @param fluxes character; either "all" or a character vector with the flux components to plot. Only used when \code{what} is "cumulative"
#' @param net logical; if TRUE, it sums the inflows and outflows of the flux component to plot the net fluxes. If FALSE, it will plot both the inflows and outflows. Only used when \code{what} is "cumulative" or "total".
#' @param type character; plot type. Either "bar" or "area".
#' @param final logical; should only the final mass budget be plotted? Defaults to FALSE
#'
#' @return ggplot2 object
#' @export
#' @method rmt_plot cbud
rmt_plot.cbud <- function(cbud,
                          btn,
                          icomp = 1,
                          what = 'cumulative',
                          fluxes = 'all',
                          net = FALSE,
                          type = 'area',
                          final = FALSE) {
  
  if(length(icomp) > 1) stop('icomp should be of length 1', call. = FALSE)
  cbud <- as.data.frame(cbud)
  cbud <- cbud[which(cbud$icomp == icomp), ]
  
  if(final) {
    cbud <- cbud[nrow(cbud),]
  } else if(nrow(cbud) == 1 && !final) {
    warning('Budget printed at only one time step. Consider setting final to TRUE.', call. = FALSE)
  }
  
  colnames(cbud)[which(colnames(cbud) == 'time')] <- 'tm'
  c_names <- !(colnames(cbud) %in% c('icomp', 'tm', 'tstp', 'kstp','kper'))

  df <- reshape(cbud, direction = 'long', varying = which(c_names), v.names = 'value', times = colnames(cbud)[c_names])
  df <- df[-8]
  colnames(df)[6] <- 'flux'
  df$io <- ifelse(endsWith(df$flux, '_in'), 'in', 'out')
  df$io <- factor(df$io)
  df$flux <- unlist(strsplit(df$flux, "\\_in|\\_out"))

  # remove all-zero fluxes
  l_zero <- aggregate(df$value, by=list(df$flux), function(i) sum(i) == 0)$x
  l_zero[which(levels(factor(df$flux)) %in% c('difference', 'discrepancy', 'total'))] <- FALSE
  df <- subset(df, df$flux %in% levels(factor(df$flux))[!l_zero])
  df$flux <- factor(df$flux)

  # plot
  
  if(what %in% c('difference', 'discrepancy', 'total')) {
    df <- subset(df, flux == what)
    
    x_label <- paste0('Time (', btn$tunit, ')')
    mass_unit <- paste0('(', btn$munit, ')')
    x <- ggplot2::sym('tm')
    gm_line <- ggplot2::geom_path()
    
    # check if ss 
    if((btn$nper == 1 && btn$ssflag) || final) {
      if(type == "area") {
        type <- 'bar'
        df$tm <- factor(df$tm)
        gm_line <- ggplot2::geom_col()
      } 
    }
    
    if(what == 'total') {
      if(net) {
        df <- aggregate(list(value = df$value), by = list(tm = df$tm),  sum)
        p <- ggplot2::ggplot(df, ggplot2::aes(x=!!x, y=value)) + gm_line +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = paste('Net total mass budget for species', icomp), y = paste('Mass', mass_unit), x = x_label)
      } else {
        if(type == 'bar') {
          p <- ggplot2::ggplot(df, ggplot2::aes(x=factor(!!x), y=value, colour=io, fill=io)) +
            ggplot2::geom_col() + 
            ggplot2::geom_hline(yintercept = 0, colour = 'black') +
            ggplot2::labs(title = paste('Gross total mass budget for species', icomp), y = paste('Mass', mass_unit), x = x_label)
          
        } else if(type == 'area') {
          p <- ggplot2::ggplot() +
            ggplot2::geom_area(data=subset(df, io=='in'), ggplot2::aes(x=!!x ,y=value), alpha=0.7) +
            ggplot2::geom_area(data=subset(df, io=='out'), ggplot2::aes(x=!!x ,y=value), alpha=0.7) +
            ggplot2::geom_hline(yintercept = 0, colour = 'black') +
            ggplot2::labs(title = paste('Gross total mass budget for species', icomp), y = paste('Mass', mass_unit), x = x_label)
          
        }
      }
    } else { # difference/discrepancy
      # plot
      p <- ggplot2::ggplot(df, ggplot2::aes(x=!!x, y=value, group=io)) + gm_line + 
        ggplot2::geom_hline(yintercept = 0, colour = 'black') +
        ggplot2::labs(title = paste(rmti_ifelse0(what == 'difference', "Mass in - Mass out", "Discrepancy"), 'for species', icomp), y = rmti_ifelse0(what == 'difference', paste('Mass', mass_unit), "% discrepancy"), x = x_label)
    }
    
    # cumulative
  } else {
    
    df <- subset(df, !(flux %in% c('difference', 'discrepancy', 'total')))
    # remove fluxes if necessary
    if(length(fluxes) > 1 || fluxes != 'all') df <- subset(df, flux %in% fluxes)
    
    x_label <- paste0('Time (', btn$tunit, ')')
    mass_unit <- paste0('(', btn$munit, ')')
    x <- ggplot2::sym('tm')
    
    # check if ss 
    if((btn$nper == 1 && btn$ssflag) || final) {
      if(type == "area") {
        type <- 'bar'
        x <- ggplot2::sym('flux')
        x_label <- 'flux'
      } else {
        df$tm <- factor(df$tm)
      }
    }
    
    # plot
    if(net) {
      df <- aggregate(list(value = df$value), by = list(tm = df$tm, flux = df$flux),  sum)
      if(type == 'bar') {
        p <- ggplot2::ggplot(df, ggplot2::aes(x=factor(!!x), y=value, colour=flux, fill=flux)) +
          ggplot2::geom_col() +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = paste("Net cumulative mass budget for species", icomp), y = paste('Mass', mass_unit), x = x_label)
      } else if(type == 'area') {
        p <- ggplot2::ggplot(data=df, ggplot2::aes(x=!!x ,y=value, colour = flux, fill = flux)) +
          ggplot2::geom_area(alpha=0.7) +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = paste("Net cumulative mass budget for species", icomp), y = paste('Mass', mass_unit), x = x_label)
      }
    } else {
      if(type == 'bar') {
        p <- ggplot2::ggplot(df, ggplot2::aes(x=factor(!!x), y=value, colour=flux, fill=flux)) +
          ggplot2::geom_col() +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = paste("Gross cumulative mass budget for species", icomp), y = paste('Mass', mass_unit), x = x_label)
        
      } else if(type == 'area') {
        p <-  ggplot2::ggplot() +
          ggplot2::geom_area(data=subset(df, io=='in'), ggplot2::aes(x=!!x ,y=value, colour = flux, fill = flux), alpha=0.7) +
          ggplot2::geom_area(data=subset(df, io=='out'), ggplot2::aes(x=!!x ,y=value, colour = flux, fill = flux), alpha=0.7) +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = paste("Gross cumulative mass budget for species", icomp), y = paste('Mass', mass_unit), x = x_label)
      }
    }
  }
  
  return(p)
  
}

#' Plot an MT3DMS ss object
#' 
#' \code{rmt_plot.ss} plots an RMT3DMS ss object
#' 
#' @param ss ss object
#' @method rmt_plot ss
#' @export
rmt_plot.ss <- function(ss) {
  ggplot2::ggplot(ss,ggplot2::aes(x=time,y=max_conc_diff))+
    ggplot2::geom_point()+
    ggplot2::geom_path()+
    ggplot2::geom_hline(yintercept=attr(ss,'threshold'),colour='gray',linetype='dashed')
}

#' Plot a MT3DMS observed concentration file
#' 
#' @param ocn \code{RMT3DMS} ocn object
#' @param type plot type: 'scatter', 'residual', 'time' or 'histogram'
#' @param cinact value used to flag dry cells; defaults to -888
#' @param bins number of bins to use in the histogram plot; defaults to the Freedman-Diaconis rule
#' @method rmt_plot ocn
#' @export
rmt_plot.ocn <- function(ocn,type='scatter',cinact = -888, bins = NULL) {
  if(!('observed' %in% colnames(ocn))) stop('No observed concentrations present in ocn object. Is ioutcobs set to 0 in the tob object?', call. = FALSE)
  dat <- data.frame(simulated=ocn$calculated, observed=ocn$observed,name=ocn$wellid,time=ocn$total_elapsed_time)[which(ocn$calculated!=cinact),]
  if('residual' %in% colnames(ocn)) {
    dat$residual <- ocn$residual[which(ocn$calculated!=cinact)]
  } else if('log_residual' %in% colnames(ocn)) {
    dat$residual <- ocn$log_residual[which(ocn$calculated!=cinact)]
  } else {
    dat$residual <- dat$simulated - dat$observed
  }
  
  if(type=='scatter') {
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=observed,y=simulated))+
               ggplot2::geom_point(ggplot2::aes(colour=abs(residual)))+
               ggplot2::geom_abline(ggplot2::aes(intercept=0,slope=1),linetype='dashed')+
               ggplot2::scale_colour_gradientn('Misfit',colours=RMODFLOW:::rmfi_rev_rainbow(7),trans='log10')+
               ggplot2::xlab('Observed value')+
               ggplot2::ylab('Simulated equivalent')
    )
  } else if(type=='residual') {
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=name,y=residual))+
               ggplot2::geom_bar(ggplot2::aes(fill=abs(residual)),stat='identity')+
               ggplot2::scale_fill_gradientn('Misfit',colours=RMODFLOW:::rmfi_rev_rainbow(7),trans='log10')+
               ggplot2::xlab('Observation name')+
               ggplot2::ylab('Simulated equivalent - observed value')
    )
  } else if(type=='histogram') {
    if(is.null(bins)) bins <- nclass.FD(dat$simulated-dat$observed)
    
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=residual))+
               ggplot2::geom_histogram(ggplot2::aes(fill=..count..), bins=bins)+
               ggplot2::scale_fill_gradientn('Count',colours=RMODFLOW:::rmfi_rev_rainbow(7))+
               ggplot2::xlab('Simulated equivalent - observed value')
    )
  } else if(type == 'time') {
    return(ggplot2::ggplot(dat) +
      ggplot2::geom_line(ggplot2::aes(x=time, y = simulated, colour = name, group = name)) +
      ggplot2::geom_point(ggplot2::aes(x=time, y=observed, colour=name), shape = 13) +
      ggplot2::labs(x = 'Total elapsed time', y = 'Concentration', colour = 'Name'))
  } else {
    stop('plot type not supported', call. = FALSE)
  }
}

#' Plot a MT3DMS mass flux file
#' 
#' @param mfx \code{RMT3DMS} mfx object
#' @param type plot type: 'scatter', 'residual', 'time' or 'histogram'
#' @param bins number of bins to use in the histogram plot; defaults to the Freedman-Diaconis rule
#' @method rmt_plot mfx
#' @export
rmt_plot.mfx <- function(mfx,type='scatter', bins = NULL) {
  if(!('observed' %in% colnames(mfx))) stop('No observed concentrations present in mfx object. Is ioutflux set to 0 in the tob object?', call. = FALSE)
  dat <- data.frame(simulated=mfx$calculated, observed=mfx$observed,name=mfx$name, time=mfx$total_elapsed_time)
  if('residual' %in% colnames(mfx)) {
    dat$residual <- mfx$residual
  } else if('log_residual' %in% colnames(mfx)) { # not possible
    dat$residual <- mfx$log_residual
  } else {
    dat$residual <- dat$simulated - dat$observed
  }
  
  if(type=='scatter') {
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=observed,y=simulated))+
               ggplot2::geom_point(ggplot2::aes(colour=abs(residual)))+
               ggplot2::geom_abline(ggplot2::aes(intercept=0,slope=1),linetype='dashed')+
               ggplot2::scale_colour_gradientn('Misfit',colours=RMODFLOW:::rmfi_rev_rainbow(7),trans='log10')+
               ggplot2::xlab('Observed value')+
               ggplot2::ylab('Simulated equivalent')
    )
  } else if(type=='residual') {
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=name,y=residual))+
               ggplot2::geom_bar(ggplot2::aes(fill=abs(residual)),stat='identity')+
               ggplot2::scale_fill_gradientn('Misfit',colours=RMODFLOW:::rmfi_rev_rainbow(7),trans='log10')+
               ggplot2::xlab('Observation name')+
               ggplot2::ylab('Simulated equivalent - observed value')
    )
  } else if(type=='histogram') {
    if(is.null(bins)) bins <- nclass.FD(dat$simulated-dat$observed)
    
    return(  ggplot2::ggplot(dat,ggplot2::aes(x=residual))+
               ggplot2::geom_histogram(ggplot2::aes(fill=..count..), bins=bins)+
               ggplot2::scale_fill_gradientn('Count',colours=RMODFLOW:::rmfi_rev_rainbow(7))+
               ggplot2::xlab('Simulated equivalent - observed value')
    )
  } else if(type == 'time') {
    return(ggplot2::ggplot(dat) +
             ggplot2::geom_line(ggplot2::aes(x=time, y = simulated, colour = name, group = name)) +
             ggplot2::geom_point(ggplot2::aes(x=time, y=observed, colour=name), shape = 13) +
             ggplot2::labs(x = 'Total elapsed time', y = 'Mass flux', colour = 'Name'))
  } else {
    stop('plot type not supported', call. = FALSE)
  }
}

#' Plot a MT3DMS 2D array
#' 
#' \code{rmt_plot.rmt3dms_2d_array} plots a MODFLOW 2D array.
#' 
#' @param rmt_2d_array an object of class rmt_2d_array
#' @param btn basic transport file object
#' @param ... arguments passed to RMODFLOW::rmf_plot.rmf_2d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmt_plot rmt_2d_array
#' @export
rmt_plot.rmt_2d_array <- function(rmt_2d_array,
                                  btn,
                                  mask = {warning('Using first icbund layer as mask.', call. = FALSE); btn$icbund[,,1]},
                                  ...) {
  dis <- rmt_convert_btn_to_dis(btn)
  RMODFLOW::rmf_plot(RMODFLOW::rmf_create_array(rmt_2d_array), dis=dis, mask = mask, ...)
}

#' Plot a 2D section through a MT3DMS 3D array
#' 
#' \code{rmt_plot.rmt_3d_array} plots a 2D section through a MT3DMS 3D array.
#' 
#' @param rmt_3d_array an object of class rmt_3d_array
#' @param btn basic transport file object
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param mask a 3D array with 0 or F indicating inactive cells; defaults to btn$icbund
#' @param ... arguments provided to RMODFLOW::rmf_plot.rmf_3d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmt_plot rmt_3d_array
#' @export
rmt_plot.rmt_3d_array <- function(rmt_3d_array,
                                  btn,
                                  i = NULL,
                                  j = NULL,
                                  k = NULL,
                                  mask = btn$icbund,
                                  ...) {
  dis <- rmt_convert_btn_to_dis(btn)
  RMODFLOW::rmf_plot(RMODFLOW::rmf_create_array(rmt_3d_array), dis = dis, mask = mask, i = i, j = j, k = k, ...)
}

#' Plot a 2D section through a MT3DMS 4D array 
#'
#' \code{rmt_plot.rmt_4d_array} plots a 2D section through a MT3DMS 4D array.
#'
#' @param rmt_4d_array an object of class rmt_4d_array
#' @param btn basic transport file object
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param l transport time step number to plot (index for 4th dimension)
#' @param mask a 3D array with 0 or F indicating inactive cells; defaults to btn$icbund
#' @param ... arguments provided to RMODFLOW::rmf_plot.rmf_3d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmt_plot rmt_4d_array
#' @export
rmt_plot.rmt_4d_array <- function(rmt_4d_array,
                                  btn,
                                  i = NULL,
                                  j = NULL,
                                  k = NULL,
                                  l = NULL,
                                  mask = btn$icbund,
                                  ...) {
  
  dis <- rmt_convert_btn_to_dis(btn)
  RMODFLOW::rmf_plot(RMODFLOW::rmf_create_array(rmt_4d_array), dis = dis, mask = mask, i = i, j = j, k = k, l = l, ...)
}

#' Plot a RMT3DMS list object
#'
#' @param obj a \code{RMT3DMS} object of class \code{rmt_list}
#' @param btn a \code{RMT3DMS} btn object
#' @param mask a 3D array with 0 or F indicating inactive cells; defaults to btn$icbund
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param variable single character or numeric indicating which column in the \code{rmt_list} object to plot. Defaults to 'id', which plots the locations of the cells.
#' @param ... additional arguments passed to either \code{\link{RMODFLOW::rmf_plot.rmf_list}}
#' 
#' @return ggplot2 object or layer
#' @method rmt_plot rmt_list
#' 
#' @export
rmt_plot.rmt_list <- function(obj, 
                              btn, 
                              mask = btn$icbund,
                              variable = 'id',
                              i = NULL,
                              j = NULL,
                              k = NULL,
                              ...) {
  dis <- rmt_convert_btn_to_dis(btn)
  RMODFLOW::rmf_plot(obj, dis = dis, i = i, j = j, k = k, variable = variable, mask = mask, ...)
}

#' Plot a RMT3DMS tob object
#'
#' @param tob \code{RMT3DMS} tob object
#' @param btn \code{RMT3DMS} btn object
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param prj projection file object
#' @param what character, either 'concentrations' (default) or 'fluxes'. Denotes which observations to plot.
#' @param ... additional arguments passed to \code{\link{rmt_plot.rmt_list}}
#' 
#' @details specifying all of the \code{i, j & k} arguments will plot a time series at that cell location. The observations are grouped by name.
#' 
#' @return ggplot2 object or layer
#' @export
#' @method rmt_plot tob
#' 
rmt_plot.tob <- function(tob,
                         btn,
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         what = 'concentrations',
                         prj = RMODFLOW::rmf_get_prj(btn),
                         ...) {
  
  if(is.null(i) && is.null(j) && is.null(k)) stop('Please provide i, j or k.', call. = FALSE)
  
  if(what == 'concentrations') {
    if(tob$inconcobs == 0) stop('No concentration observations specified in tob', call. = FALSE) 
    locations <- rmt_convert_tob_to_locations(tob, btn, prj = prj)
  } else if(what == 'fluxes') {
    if(tob$influxobs == 0) stop('No mass flux observations specified in tob', call. = FALSE) 
    locations <- tob$fluxcells
  } else {
    stop('what should be either concentrations or fluxes', call. = FALSE)
  }

  # plot time series
  if(!is.null(i) && !is.null(j) && !is.null(k)) {
    locations <- locations[which(locations$i == i & locations$j == j & locations$k == k),]
    if(what == 'concentrations') {
      ts <- tob$concentrations
      ts <- ts[which(ts$cobsnam %in% locations$name),]
      names(ts)[which(names(ts) == 'cobs')] <- 'value'
    } else {
      ts <- tob$fluxobs
      ts <- ts[which(ts$group %in% locations$group),]
      names(ts)[which(names(ts) == 'flux')] <- 'value'
    }

    mp <- any(table(ts$name) > 1)
    
    return(ggplot2::ggplot(ts, ggplot2::aes(x = time, y = value, group = name)) +
             rmti_ifelse0(mp, ggplot2::geom_path(ggplot2::aes(colour = name)), ggplot2::geom_point(ggplot2::aes(colour = name))))
  } else {
    df <- RMODFLOW::rmf_create_list(locations)
    dis <- rmt_convert_btn_to_dis(btn, prj = prj)
    
    # rmf_plot.rmf_list
    
    if(what == 'concentrations') df <- df[, -which(colnames(df) %in% c('x', 'y', 'z'))]
    RMODFLOW::rmf_plot(df, dis = dis, i = i, j = j, k = k, prj = prj, ...)
    
  }
}

#' Plot a MT3D ucn file object
#'
#' @param ucn \code{RMT3DMS} ucn object as returned from \code{rmt_read_ucn}
#' @param btn \code{RMT3DMS} btn object
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param l time step number to plot (index for 4th dimension); defaults to plotting the final time step. See details.
#' @param ntrans integer specifying the transport time step to plot. See details. 
#' @param ... additional parameters passed to \code{\link{rmt_plot.rmf_4d_array}}
#'
#' @details There are two ways to specify which time step to plot:
#'  The \code{l} argument can be specified which represents the dimension index. This can be smaller than the total number of transport time steps since output may not be written for all steps. 
#'  The second option is to specify \code{ntrans} which is the total transport time step number. Since output at every time step is not always written, a specified ntrans value might not be available in the array.
#'  
#'  If no output is written for the specified time step, as controlled by the btn object, an error is thrown.
#'
#' @return ggplot2 object or layer
#' @method rmt_plot ucn
#' @export
#'
rmt_plot.ucn <- function(ucn, 
                         btn,
                         i = NULL,
                         j = NULL,
                         k = NULL,
                         l = NULL,
                         ntrans = NULL,
                         ...) {
  
  if(inherits(ucn, 'rmt_4d_array')) {
    # skip if ijk are specified and a time series should be plotted
    if(!(!is.null(i) && !is.null(j) && !is.null(k))) {
      if(is.null(l) && is.null(ntrans)) {
        if(dim(ucn)[4] > 1) warning('Plotting final transport time step results.', call. = FALSE)
        l <- dim(ucn)[4]
      } else if(is.null(l) && !is.null(ntrans)) {
        if(!(ntrans %in% attr(ucn, 'ntrans'))) stop('No output written for specified ntrans transport time step.', call. = FALSE)
        l <- which(attr(ucn, 'ntrans') == ntrans)
      }
      
      rmt_plot.rmt_4d_array(ucn, btn = btn, i=i, j=j, k=k, l=l, ...)
      
    } else {
      rmt_plot.rmt_4d_array(ucn, btn = btn, i=i, j=j, k=k, l=l, ...)
    }
  } else if(inherits(ucn, 'rmt_3d_array')) {
    rmt_plot.rmt_3d_array(ucn, btn = btn, i=i, j=j, k=k, ...)
  } else if(inherits(ucn, 'rmt_2d_array')) {
    rmt_plot.rmt_2d_array(ucn, btn = btn, i=i, j=j, ...)
  } else {
    stop('Array is not of class rmt_2d_array, rmt_3d_array or rmt_4d_array. Is the array subsetted ?', call. = FALSE)
  }
}
  

#' Plot a MT3DMS mass summary file time series
#'
#' @param mas \code{RMT3DMS} mas object as returned from \code{rmt_read_mas}
#' @param variable character specifying which variable to plot. Defaults to 'all'. Allowed values are the colnames of \code{mas}.
#' @param final logical, should only the final time step be plotted. Default to FALSE.
#'
#' @details plots a time series line plot of the variable. If \code{variable = 'all'}, \code{ggplot2::facet_wrap} is used to plot all variable with free y scales. 
#'  If \code{final = TRUE}, a bar plot is returned using \code{ggplot2::geom_col} of the final time step.
#'  
#' @return ggplot2 object
#' @method rmt_plot mas
#' @export
#'
rmt_plot.mas <- function(mas, 
                         variable = 'all',
                         final = FALSE) {
  
  if(length(variable) != 1) stop('variable should be of length 1', call. = FALSE)
  type <- 'line'
  if(final) {
    type <- 'bar'
    mas <- mas[nrow(mas),]
  }
  if(variable == 'all') {
    colnames(mas)[which(colnames(mas) == 'time')] <- 'tm'
    df <- reshape(as.data.frame(mas), direction = 'long', varying = colnames(mas)[-1], v.names = 'value', timevar = 'variable', times = colnames(mas)[-1])
    colnames(df)[which(colnames(df) == 'tm')] <- 'time'
    
    if(type == 'line') {
      ggplot2::ggplot(df) +
        ggplot2::geom_line(ggplot2::aes(time, value, colour = variable, group = variable)) +
        ggplot2::facet_wrap(~variable, scales = 'free_y')
    } else if(type == 'bar') {
      ggplot2::ggplot(df) +
        ggplot2::geom_col(ggplot2::aes(time, value, fill = variable)) +
        ggplot2::facet_wrap(~variable, scales = 'free_y')
    } else {
      stop('type should be line or bar', call. = FALSE)
    }

    
  } else {
    df <- mas[, c(1, which(colnames(mas) == variable))]
    if(type == 'line') {
      ggplot2::ggplot(df) +
        ggplot2::geom_line(ggplot2::aes_string('time', variable))
    } else if(type == 'bar') {
      ggplot2::ggplot(df) +
        ggplot2::geom_col(ggplot2::aes_string('time', variable)) 
    } else {
      stop('type should be line or bar', call. = FALSE)
    }
  }
  
}

#' Plot a MT3DMS concentrations observation time series
#'
#' @param cobs \code{RMT3DMS} cobs objected as returned from \code{rmt_read_obs}
#' @param i row number to subset observations. Defaults to NULL.
#' @param j column number subset observations. Defaults to NULL.
#' @param k layer number subset observations. Defaults to NULL. 
#' 
#' @details The observations are named using their k-i-j indices.
#' 
#' @return ggplot2 object
#' @export
#' @method rmt_plot cobs
#'
rmt_plot.cobs <- function(cobs,
                          i = NULL,
                          j = NULL,
                          k = NULL) {
  
  cobs <- as.data.frame(cobs)
  cobs$name <- paste(cobs$k, cobs$i, cobs$j, sep = '-')
  if(!is.null(k)) cobs <- cobs[which(cobs$k %in% k),]
  if(!is.null(i)) cobs <- cobs[which(cobs$i %in% i),]
  if(!is.null(j)) cobs <- cobs[which(cobs$j %in% j),]

  ggplot2::ggplot(cobs) +
    ggplot2::geom_line(ggplot2::aes(time, value, colour = name, group = name)) +
    ggplot2::labs(colour = 'k-i-j')
  
}

# # Do this??
# rmt_plot.ssm 




