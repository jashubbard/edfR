\name{edf.events}
\alias{edf.events}
\title{Load Events}
\description{
Loads the events of a SR EyeLink EDFfile.
}
\usage{
edf.events(EDFfile, type, fields)
}
\arguments{
  \item{EDFfile}{a character string giving the
    name of the EDFfile to load.}
  \item{type}{a character string array giving the
    name of the event types to load.}
  \item{fields}{a character string array giving the
    names of the fields to load.}
}
\details{
\bold{type} may be:
"STARTBLINK", "STARTSACC", "STARTFIX", "STARTSAMPLES", "STARTEVENTS", "STARTPARSE", "ENDBLINK", "ENDSACC", "ENDFIX", "ENDSAMPLES", "ENDEVENTS", "ENDPARSE", "FIXUPDATE", "BREAKPARSE", "BUTTONEVENT", "INPUTEVENT", "MESSAGEEVENT", "SAMPLE_TYPE", "RECORDING_INFO" (default=ENDFIX.)

\bold{fields} may be:
\itemize{
\item{"time": effective time of event}
\item{"type": 1=STARTPARSE, 2=ENDPARSE, 3=STARTBLINK, 4=ENDBLINK, 5=STARTSACC, 6=ENDSACC, 7=STARTFIX, 8=ENDFIX, 17=STARTEVENTS, 18=ENDEVENTS, 24=MESSAGEEVENT, 25=BUTTONEVENT}
\item{"read": flags which items were included}
\item{"eye": 0=left, 1=right}
\item{"sttime": start time of the event}
\item{"entime": end time of the event}
\item{"hstx", "hsty": headref starting points}
\item{"gstx", "gsty": gaze starting points}
\item{"sta"}
\item{"henx", "heny": headref ending points}
\item{"genx", "geny": gaze ending points}
\item{"ena"}
\item{"havx", "havy": headref averages}
\item{"gavx", "gavy": gaze averages}
\item{"ava"}
\item{"avel": accumulated average velocity}
\item{"pvel": accumulated peak velocity}
\item{"svel", "evel": start, end velocity}
\item{"supd_x", "eupd_x": start, end units-per-degree}
\item{"supd_y", "eupd_y": start, end units-per-degree}
\item{"status": error, warning flags}
\item{"flags": error, warning flags}
\item{"input"}
\item{"buttons"}
\item{"parsedby": 7 bits of flags: PARSEDBY codes}
\item{default is c("sttime","entime","gavx","gavy")}
}
Specification of the field values are copied from the EDF Access C API user manual.
}
\value{
a dataframe containing the specified fields of the events of a given type.
}
%\references{}
%\note{}
\author{Guenther, T. and von der Malsburg, T.}
%\seealso{}
\examples{
\dontrun{

#this returns the start, end, (average) x coordinates, and (average) y coordinates of fixations:
edf.events('example.EDF')

#this gets the relevant information for saccades
events <- edf.events("example.EDF",type='ENDSACC',
            fields=c('sttime','entime','gstx','gsty','genx','geny','avel','pvel'))

}}
%\keyword{}
