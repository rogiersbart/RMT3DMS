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
  cbud <- cbud[which(cbud$icomp == icomp), ]
  
  if(final) cbud <- cbud[nrow(cbud),]
  
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
          ggplot2::labs(title = paste('Net total mass budget'), y = paste('Mass', mass_unit), x = x_label)
      } else {
        if(type == 'bar') {
          p <- ggplot2::ggplot(df, ggplot2::aes(x=factor(!!x), y=value, colour=io, fill=io)) +
            ggplot2::geom_col() + 
            ggplot2::geom_hline(yintercept = 0, colour = 'black') +
            ggplot2::labs(title = 'Gross total mass budget', y = paste('Mass', mass_unit), x = x_label)
          
        } else if(type == 'area') {
          p <- ggplot2::ggplot() +
            ggplot2::geom_area(data=subset(df, io=='in'), ggplot2::aes(x=!!x ,y=value), alpha=0.7) +
            ggplot2::geom_area(data=subset(df, io=='out'), ggplot2::aes(x=!!x ,y=value), alpha=0.7) +
            ggplot2::geom_hline(yintercept = 0, colour = 'black') +
            ggplot2::labs(title = 'Gross total mass budget', y = paste('Mass', mass_unit), x = x_label)
          
        }
      }
    } else { # difference/discrepancy
      # plot
      p <- ggplot2::ggplot(df, ggplot2::aes(x=!!x, y=value, group=io)) + gm_line + 
        ggplot2::geom_hline(yintercept = 0, colour = 'black') +
        ggplot2::labs(title = rmti_ifelse0(what == 'difference', "Mass in - Mass out", "Discrepancy"), y = rmti_ifelse0(what == 'difference', paste('Mass', mass_unit), "% discrepancy"), x = x_label)
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
          ggplot2::labs(title = "Net cumulative mass budget", y = paste('Mass', mass_unit), x = x_label)
      } else if(type == 'area') {
        p <- ggplot2::ggplot(data=df, ggplot2::aes(x=!!x ,y=value, colour = flux, fill = flux)) +
          ggplot2::geom_area(alpha=0.7) +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = "Net cumulative mass budget", y = paste('Mass', mass_unit), x = x_label)
      }
    } else {
      if(type == 'bar') {
        p <- ggplot2::ggplot(df, ggplot2::aes(x=factor(!!x), y=value, colour=flux, fill=flux)) +
          ggplot2::geom_col() +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = "Gross cumulative mass budget", y = paste('Mass', mass_unit), x = x_label)
        
      } else if(type == 'area') {
        p <-  ggplot2::ggplot() +
          ggplot2::geom_area(data=subset(df, io=='in'), ggplot2::aes(x=!!x ,y=value, colour = flux, fill = flux), alpha=0.7) +
          ggplot2::geom_area(data=subset(df, io=='out'), ggplot2::aes(x=!!x ,y=value, colour = flux, fill = flux), alpha=0.7) +
          ggplot2::geom_hline(yintercept = 0, colour = 'black') +
          ggplot2::labs(title = "Gross cumulative mass budget", y = paste('Mass', mass_unit), x = x_label)
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
#' @param l transport time step number to plot
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
  RMODFLOW::rmt_plot(RMODFLOW::rmf_create_array(rmt_4d_array), dis = dis, mask = mask, i = i, j = j, k = k, l = l, ...)
}






