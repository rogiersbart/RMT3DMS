% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-rct.R
\name{rmt_create_rct}
\alias{rmt_create_rct}
\title{Create an \code{RMT3DMS} rct object}
\usage{
rmt_create_rct(
  btn,
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
  rec_filename = "reaction_module.dat",
  ned = 1,
  nea = 5,
  nspecial = 0,
  ifesld = 0,
  ispec = rep(0, nspecial),
  specialispec = rep("SOLID", nspecial),
  efcmax = rep(0, nspecial),
  hsc = rep(0.5, nea),
  ic = rep(0.01, nea),
  decayrate = matrix(0.0186, nrow = nea, ncol = ned),
  yieldc = matrix(1, nrow = nea + ned, ncol = ned)
)
}
\arguments{
\item{btn}{\code{RMT3DMS} btn object}

\item{isothm}{integer flag to set type of sorption (or dual-domain mass transfer). See MT3DMS/MT3D-USGS manual. Defaults to 0.}

\item{ireact}{integer flag indicating which type of kinetic rate reaction is simulated. See MT3DMS/MT3D-USGS manual. Defaults to 0.}

\item{irctop}{integer flag indicating how reaction variables are entered. See MT3DMS/MT3D-USGS manual. Defaults to 2 (as 3D-arrays).}

\item{igetsc}{integer flag specifying if initial concentration of nonequilibrium sorbed or immobile phases should be read. See MT3DMS/MT3D-USGS manual. Defaults to 0 (not read).}

\item{ireaction}{MT3D-USGS only. Integer flag specifying a reaction module. See MT3D-USGS manual. Defaults to 0 (no reaction module.}

\item{rhob}{3d array with bulk density of aquifer medium. Defaults to 157 for all cells.}

\item{prsity2}{3d array with porosity values for the immobile domain. Defaults to 0.1 for all cells.}

\item{srconc}{list with \code{ncomp} 3d arrays specifying the initial sorbed concentrations if \code{igetsc > 0}. Defaults to 0 for all cells.}

\item{sp1}{list with \code{ncomp} 3d arrays specifying the first sorption parameter. See MT3DMS/MT3D-USGS manual. Defaults to 0.}

\item{sp1im}{list with \code{ncomp} 3d arrays speciyfing the immobile domain distribution coefficient for sorption. See MT3DMS/MT3D-USGS manual. Defaults to 0.}

\item{sp2}{list with \code{ncomp} 3d arrays specifying the second sorption or dual-domain model parameter. See MT3DMS/MT3D-USGS manual. Defaults to 0.}

\item{rc1}{list with \code{ncomp} 3d arrays specifying the primary kinetic parameter. See MT3DMS/MT3D-USGS manual. Defaults to 0.}

\item{rc2}{list with \code{ncomp} 3d arrays specifying the secondary kinetic parameter.See MT3DMS/MT3D-USGS manual. Defaults to 0.}

\item{rc3}{list with \code{ncomp} 3d arrays specifying the half-saturation constants when \code{ireact = 2}. Defaults to 0.}

\item{yld}{numeric vector of length \code{ncomp - 1} with the yield coefficients between species when \code{ireact = 3}. \code{ncomp} should be larger than 1 when this feature is used. See MT3DMS/MT3D-USGS manual.}

\item{ied}{integer species number representing the electron donor in a EA/ED reaction (\code{ireaction = 1}).}

\item{iea}{integer species number representing the electron acceptor in a EA/ED reaction (\code{ireaction = 1}).}

\item{f}{stoiciometric ratio in the simulated equation \code{ED + F*EA -> Product} (\code{ireaction = 1}).}

\item{rec_filename}{character, name of the input file that will provide parameter information when \code{ireaction = 2}. Defaults to \code{'reaction_module.dat'}.}

\item{ned}{number of electron donors when \code{ireaction = 2}.}

\item{nea}{number of electron acceptors when \code{ireaction = 2}.}

\item{nspecial}{number of special cases when \code{ireaction = 2}.}

\item{ifesld}{integer flag specifying if solid phase Fe(III) is simulated. Defaults to 0 (not simulated).}

\item{ispec}{numeric vector of length \code{nspecial} specifying the sequential order of the species treated as special case.}

\item{specialispec}{character vector of length \code{nspecial} specifying the keyword of the species treated as special case. See MT3D-USGS manual.}

\item{efcmax}{numeric vector of length \code{nspecial} specifying the maximum express field capacity (EFC) of the species treated as special case. See MT3D-USGS manual.}

\item{hsc}{numeric vector of length \code{nea} with the half-saturation constants.}

\item{ic}{numeric vector of length \code{nea} with the inhibition constants.}

\item{decayrate}{matrix with \code{nea} rows and \code{ned} columns specifying the decay rates of each EA corresponding to each ED.}

\item{yieldc}{matrix with \code{nea + ned} rows and \code{ned} columns specifying the yield coefficient of each component corresponding to each ED.}
}
\value{
an object of class \code{rct}
}
\description{
\code{rmt_create_rct} creates an \code{RMT3DMS} rct object.
}
\details{
If \code{irctop < 2}, all 3D variables should be given as 1D vectors with one value per layer.
}
\seealso{
\code{\link{rmt_read_rct}}, \code{\link{rmt_write_rct}}
}
