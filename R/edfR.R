#' edfR: Import from SR Eyelink EDF files
#'
#' @description
#' edfR allows one to import data from SR Research Eyelink EDF files. This includes
#' event-wise (fixations, saccades, blinks) as well as continuous samples (e.g., pupil diameter).
#'
#' @section Functions:
#'
#' @section Basic Importing:
#' \itemize{
#' \item \code{\link{edf.events}} imports events (fixations by default, but any event can be specified).
#' \item \code{\link{edf.samples}} imports sample data including gaze location and pupil diameter
#' \item \code{\link{edf.messages}} imports messages, including those sent to Eyelink during an experiment
#' \item \code{\link{edf.recordings}} imports recording information.
#' }
#' @section Importing More Information:
#' \itemize{
#' \item \code{\link{edf.all}} imports events, messages, and (optionally) samples
#' \item \code{\link{edf.trials}} does the same as \code{edf.all} but in a trial-wise manner. Trials are determined
#' by the starting and stopping of the eye tracker during an experiment. It also adds an 'eyetrial' variable to
#' all events/samples to facilitate merging with behavioral data
#' }
#'
#' @section Utilities:
#' \itemize{
#' \item \code{\link{edf.trialcount}} quickly returns the number of trials in a named EDF file with little overhead
#' }
#' @author Jason Hubbard, \email{hubbard3@@uoregon.edu}, Guenther, T. and von der Malsburg, T.
#'
#' @docType package
#' @name edfR
NULL
#> NULL
