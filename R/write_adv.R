#' Write an MT3DMS file
#' 
#' @export
write_adv <- function(adv, file, IPRN=-1)
{
  # Data set B1
    cat(paste0(c(prettyNum(c(adv$MIXELM,adv$PERCEL,adv$MXPART,adv$NADVFD),width=10), '\n'),collapse=''), file=file)
  
  # Data set B2
    if(adv$MIXELM %in% c(1,2,3))
    {  
      cat(paste0(c(prettyNum(c(adv$ITRACK,adv$WD),width=10), '\n'),collapse=''), file=file, append=TRUE)
    }
  
  # Data set B3
    if(adv$MIXELM %in% c(1,3))
    {  
      cat(paste0(c(prettyNum(c(adv$DCEPS,adv$NPLANE,adv$NPL,adv$NPH,adv$NPMIN,adv$NPMAX),width=10), '\n'),collapse=''), file=file, append=TRUE)
    }
  
  # Data set B4
    if(adv$MIXELM %in% c(2,3))
    {  
      cat(paste0(c(prettyNum(c(adv$INTERP,adv$NLSINK,adv$NPSINK),width=10), '\n'),collapse=''), file=file, append=TRUE)
    }
  
  # Data set B5
    if(adv$MIXELM==3)
    {  
      cat(paste0(c(prettyNum(c(adv$DCHMOC),width=10), '\n'),collapse=''), file=file, append=TRUE)
    }  
}