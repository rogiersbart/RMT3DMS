#' Run an MT3DMS model
#' 
#' \code{run_mt3dms_ss} keeps on running an MT3DMS model until convergence is reached
#' 
#' @param file Path to name file; typically "*.nam"
#' @export
run_mt3dms_ss <- function(file,threshold,report=TRUE,report_plot=TRUE,...)
{
  dir <- dirname(file)
  file <- basename(file)
  ss <- data.frame(run=0,time=0,max_conc_diff=NA)
  attr(ss,'threshold') <- threshold
  class(ss) <- c('ss','data.frame')
  convergence <- FALSE
  while(!convergence)
  {
    run_mt3dms(paste0(dir,'/',file),...)
    nam <- read_nam(paste0(dir,'/',file))
    btn <- read_btn(paste0(dir,'/',nam$Fname[which(nam$Ftype=='BTN')]))
    if(201 %in% nam$Nunit) 
    {
      ucn <- read_ucn(paste0(dir,'/',nam$Fname[which(nam$Nunit==201)]),btn=btn)
    } else {
      ucn <- read_ucn(paste0(dir,'/MT3D001S.UCN'),btn=btn)  
    }
    ss <- rbind(ss,c(tail(ss[,1],1)+1,tail(ss[,2],1)+btn$PERLEN,max(btn$SCONC[[1]]-ucn$CNEW[[1]],na.rm=TRUE)))
    if(tail(ss[,3],1) <= threshold) convergence <- TRUE
    if(report) print(tail(ss,1),row.names=FALSE)
    if(report_plot) print(plot(ss))
    if(!convergence)
    {
      btn$SCONC[[1]][which(!is.na(ucn$CNEW[[1]]))] <- ucn$CNEW[[1]][which(!is.na(ucn$CNEW[[1]]))]
      write_btn(btn,file=paste0(dir,'/',nam$Fname[which(nam$Ftype=='BTN')]))
    }
  }
  return(ss)
}