#' Plot an MT3DMS ss object
#' 
#' \code{plot.ss} plots an MT3DMS ss object.
#' 
#' @method plot ss
#' @export
#' @import ggplot2
plot.ss <- function(ss)
{
  ggplot(ss,aes(x=time,y=max_conc_diff))+
    geom_point()+
    geom_path()+
    geom_hline(yintercept=attr(ss,'threshold'),colour='gray',linetype='dashed')
}