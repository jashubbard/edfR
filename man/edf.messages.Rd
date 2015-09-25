\name{edf.messages}
\alias{edf.messages}
\title{Load Messages}
\description{
Loads the messages of a SR EyeLink EDFfile.
}
\usage{
edf.messages(EDFfile)
}
\arguments{
  \item{EDFfile}{a character string giving the
    name of the EDFfile to load.}
}
%\details{}
\value{
a dataframe containing timestamp and messages.
}
%\references{}
%\note{}
\author{Guenther, T. and von der Malsburg, T.}
%\seealso{}
\examples{
\dontrun{
edf.messages("example.EDF")

edf.messages(EDFfile="example.EDF")
}}
%\keyword{}
