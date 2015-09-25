\name{edf.preamble}
\alias{edf.preamble}
\title{Load Preamble}
\description{
Loads the preamble of a SR EyeLink EDFfile.
}
\usage{
edf.preamble(EDFfile)
}
\arguments{
  \item{EDFfile}{a character string giving the
    name of the EDFfile to load.}
}
%\details{}
\value{
a string containing the preamble.
}
%\references{}
%\note{}
\author{Guenther, T. and von der Malsburg, T.}
%\seealso{}
\examples{
\dontrun{
edf.preamble("example.EDF")

edf.preamble(EDFfile="example.EDF")
}}
%\keyword{}
