\name{edf.samples}
\alias{edf.samples}
\title{Load Samples}
\description{
Loads the samples of a SR EyeLink EDFfile.
}
\usage{
edf.samples(EDFfile, fields)
}
\arguments{
  \item{EDFfile}{a character string giving the
    name of the EDFfile to load.}
  \item{fields}{a character string array giving the
    names of the fields to load.}
  \item{eventmask}{logical indicating whether to add
    an \code{\link{eventmask}} to the samples}
  \item{trials}{logical indicating whether to include
    trial information (will add a variable called \code{eyetrial}}
}
\details{
\bold{fields} may be:
\itemize{
\item{"time": time stamp of sample}
\item{"flags": flags to indicate contents}
\item{"pxL": left pupil x}
\item{"pxR": right pupil x}
\item{"pyL": left pupil y}
\item{"pyR": right pupil y}
\item{"hxL", "hxR", "hyL", "hyR": headref x, y}
\item{"paL", "paR": pupil size or area}
\item{"gxL", "gxR", "gyL", "gyR": screen gaze x, y}
\item{"rx", "ry": screen pixels per degree}
\item{"status": tracker status flags}
\item{"errors": process error flags}
}
Specification of the field values are copied from the EDF Access C API user manual.
}
\value{
a dataframe containing the specified fields of the samples.
}
%\references{}
%\note{}
\author{Guenther, T., Jason Hubbard, and von der Malsburg, T.}
%\seealso{}
\examples{
\dontrun{
edf.samples("example.EDF",
	c("time","gxL","gyL","paL","gxR","gyR","paR"))

edf.samples(EDFfile="example.EDF",
	fields=c("time","gxL","gyL","paL","gxR","gyR","paR"))

#imports trial information
edf.samples(EDFfile='example.EDF', trials=T)
}}
%\keyword{}
