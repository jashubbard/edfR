\name{edf.recordings}
\alias{edf.recordings}
\title{Load Recordings}
\description{
Loads the recordings of a SR EyeLink EDFfile.
}
\usage{
edf.recordings(EDFfile, fields)
}
\arguments{
  \item{EDFfile}{a character string giving the
    name of the EDFfile to load.}
  \item{fields}{a character string array giving the
    names of the fields to load.}
}
\details{
\bold{fields} may be:
\itemize{
\item{"time": start time or end time}
\item{"state": 0 = END, 1=START}
\item{"record_type": 1 = SAMPLES, 2= EVENTS, 3= SAMPLES and EVENTS}
\item{"pupil_type": 0 = AREA, 1 = DIAMETER}
\item{"recording_mode": 0 = PUPIL, 1 = CR}
\item{"filter_type": 1, 2, 3}
\item{"sample_rate": 250 or 500}
\item{"pos_type": 0 = GAZE, 1= HREF, 2 = RAW}
\item{"eye": 1=LEFT, 2=RIGHT, 3=LEFT and RIGHT}
}
Specification of the field values are copied from the EDF Access C API user manual.
}
\value{
a dataframe containing the specified fields of the recordings.
}
%\references{}
%\note{}
\author{Guenther, T. and von der Malsburg, T.}
%\seealso{}
\examples{
\dontrun{
edf.recordings("example.EDF", c("time","state","record_type"))

edf.recordings(EDFfile="example.EDF", 
	fields=c("time","state","pupil_type","recording_mode","filter_type","eye"))
}}
%\keyword{}
