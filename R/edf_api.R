# this code is heavily based on code from Titus von der Malsburg: http://pages.ucsd.edu/~tvondermalsburg/
edf.preamble <- function(EDFfile)
{
  .Call("get_preamble",EDFfile)
}

#' @title Load all data from EDF file
#'
#' @description
#' \code{edf.all} returns all of the most common information from
#' an SR-Research EyeLink EDF file (fixations, saccades, blinks, and samples)
#'
#' @details
#' edf.all is useful for obtaining fixations, saccades, blinks, and (optionally)
#' samples from an EDF in one shot. If you need only 1 of these (i.e., just fixations)
#' then use \code{\link{edf.events}}, \code{\link{edf.samples}}, \code{\link{edf.messages}}, or
#' \code{\link{edf.recordings}}. By default it grabs only event data. Use the \code{samples}
#'  argument to get sample data as well.
#'
#' @param EDFfile path to an EDF file
#' @param samples logical indicating whether to import samples (default=FALSE)
#' @param eventmask logical indicating whether to add an \code{\link{eventmask}} to samples (default=FALSE)
#'
#' @return The output will be a list with 4 named elements (fixations, saccades, blinks, and samples)
#' each element being a data frame
#'
#' @author Jason Hubbard, \email{hubbard3@@uoregon.edu}
#'
#' @examples
#' \dontrun{
#' output <- edf.all('/path/to/file.edf',samples=TRUE)
#' output$fixations #data frame
#' output$saccades #another data frame
#'
#' }
#'
edf.all <- function(EDFfile,samples=FALSE,eventmask=FALSE)
{
  # holder for data
  data <- list()
  # expand relative paths
  EDFfile <- path.expand((EDFfile))

  # get samples if we ask for them (otherwise it's too slow)
  if(samples){

    samp <- edf.samples(EDFfile,eventmask=eventmask)
    data$samples <- samp
  }

  # get the event data we want
  events <- edf.events(EDFfile,type=c("ENDFIX",'ENDSACC','ENDBLINK'),fields=c('type','sttime','entime','gavx','gavy','gstx','gsty','genx','geny','avel','pvel','message'))

  # subset out the particular fixations, blinks, saccades and the relevant fields
  data$blinks <- subset(events,type==4)[c('sttime','entime')]
  data$saccades <- subset(events,type==6)[c('sttime','entime','gstx','gsty','genx','geny','avel','pvel')]
  data$fixations <- subset(events,type==8)[c('sttime','entime','gavx','gavy')]

  # get messages
  messages <- edf.messages(EDFfile)
  data$messages <- messages

  data

}


#' @title Load trial-based data from an EDF file
#'
#' @description
#' \code{edf.trials} loads information from an EDF file in a
#' trial-by-trial manner. A "trial" is determined by starting
#' and stopping recording from the eye tracker during an experiment
#'
#'
#' @details
#' \code{edf.trials} will load fixations, saccades, blinks, and (optionally)
#' samples from an EDF similarly to \code{\link{edf.all}}. The difference is that
#' data are loaded based on separate trials within the recording session. A trial is
#' determined by stopping and starting recording. This is most useful for merging with
#' behavioral data recorded elsewhere. Each data frame has an \code{eyetrial} variable
#' that corresponds to the recording number.
#'
#' @param EDFfile path to an EDF file
#' @param samples logical indicating whether to import samples (default=FALSE)
#' @param eventmask logical indicating whether to add an \code{\link{eventmask}} to samples (default=FALSE)
#'
#' @return The output will be a list with 5 named elements (headers,fixations, saccades, blinks, and samples)
#' each element being a data frame. Headers indicate the starting and stopping point for each trial. Each data
#' frame will have the variable \code{eyetrial} that indicates the trial number for that event/sample.
#'
#' @author Jason Hubbard, \email{hubbard3@@uoregon.edu}
#'
#' @examples
#' \dontrun{
#' output <- edf.trials('/path/to/file.edf',samples=TRUE)
#' output$fixations #data frame
#' output$saccades #another data frame
#'
#' }
edf.trials <- function(EDFfile,samples=FALSE,eventmask=FALSE,
                       event.fields=c("time", "type", "read", "eye", "sttime", "entime","gstx", "gsty",
                                      "genx", "geny", "gavx", "gavy", "avel","pvel"),
                       sample.fields= c("time", "gxL","gyL","paL","gxR","gyR","paR"))
{
  #getting trial-by-trial data (useful for merging with behavioral data)
  EDFfile <- path.expand((EDFfile))

  R.utils::printf('Loading file %s\n',EDFfile)
  # holder for our output
  output = list()

  data <- .Call("get_trial_data",EDFfile,event.fields,sample.fields,0)

  if(!is.null(data)){
  # grab headers, do some resahping to make a nice data frame
  output$headers <- data[[1]]
  output$headers <- apply(output$headers,2,as.numeric)
  output$headers <- data.frame(output$headers)
  names(output$headers) <- c('eyetrial','starttime','endtime','duration')

  # do the same with events
  events <- as.data.frame(do.call(rbind,data[[2]]))
  names(events) <- c(event.fields,'eyetrial')
  # then subset for our fixations, saccades, and blinks (with relevant fields)
  output$fixations <- subset(events,type==8)[,c('eyetrial','sttime','entime','gavx','gavy')]
  output$saccades <- subset(events,type==6)[,c('eyetrial','sttime','entime','gstx','gsty','genx','geny','avel','pvel')]
  output$blinks <- subset(events,type==4)[,c('eyetrial','sttime','entime')]

 # and messages. do some reshaping because it starts as nested lists
  messages <- data[[3]]
  messages <- lapply(messages,as.character)
  messages <- unlist(messages)
  messages[messages=='NULL'] <- NA
  events$message <- messages
  output$messages <- events[!is.na(events$message),c('eyetrial','sttime','message')]

  if(samples)
  {
    # grab samples if we ask for them, make into a data TABLE (faster for larger datasets)
    output$samples <- edf.samples(EDFfile,fields = sample.fields,trials = TRUE, eventmask = F)

    rm(data) #free up memory

    #reorder columns
    data.table::setcolorder(output$samples,c('eyetrial',sample.fields)) #make trial first variable

    if(eventmask)
      output$samples <- eventmask(trials=output) #add our event mask if we ask for it-- faster this way
  }
  else
    output$samples <- NULL
  }
  else{

    output <- NULL
  }

  return(output)

}

edf.samples <- function(EDFfile, fields=c("time","flags","gxL","gyL","paL","gxR","gyR","paR"), eventmask=F,trials=F)
{
  EDFfile <- path.expand((EDFfile))
  # Check fields:
  fields <- unique(as.character(fields))
  valid.fields <- c("time", "flags", "pxL", "pxR", "pyL", "pyR", "hxL",
                    "hxR", "hyL", "hyR", "paL", "paR", "gxL", "gxR", "gyL",
                    "gyR", "rx", "ry", "errors")  # "status" removed in newer version of libedfapi
  for (f in fields)
    if (!f %in% valid.fields)
      stop("Request for unknown field: ", f)


  # Check EDFfile:
  EDFfile <- as.character(EDFfile)
  if (!file.exists(EDFfile))
    stop("File does not exist: ", EDFfile)

  if(trials)
    data <- data.table::as.data.table(edf.samples.trialwise(EDFfile, fields))
  else
    data <- data.table::as.data.table(edf.samples.c(EDFfile, fields))

  #make extra-super sure that it's a data table, otherwise := doesn't work
  if(!data.table::is.data.table(data))
      data <- data.table::as.data.table(data)

  if(trials)
    fields <- c(fields,'eyetrial')

  # data <- data.table::as.data.table(data)
  data.table::setnames(data,names(data),fields)
  data[,time := as.integer(time)]
  # colnames(data) <- fields
  # data$time <- as.integer(data$time)

  #tihs adds an "event mask". Binary vectors indicating whether fixation/saccade/blink occurred for each sample
  if(eventmask)
  {
   data <- eventmask(EDFfile,data)
  }


  return(data)
}

edf.samples.c <- function(EDFfile, fields)
{
  time1 <- Sys.time()
  data <- .Call("get_samples",EDFfile, fields)
  if (is.null(data))
    stop("Reading file ", EDFfile, " failed.")
  time2 <- Sys.time()
  #cat("(", time2-time1, "seconds)\n")
  data
}

edf.samples.trialwise <- function(EDFfile, fields)
{
  data <- .Call("get_samples_trialwise",EDFfile, fields,TRUE)
  if (is.null(data))
    stop("Reading file ", EDFfile, " failed.")
  data
}


edf.recordings <- function(EDFfile, fields=c("time","state","record_type","pupil_type","recording_mode","filter_type","sample_rate","pos_type","eye"))
{
  EDFfile <- path.expand((EDFfile))
  edf.recordings.c(EDFfile,fields)
}

edf.recordings.c <- function(EDFfile,fields)
{
  data <- .Call("get_recordings",EDFfile,unique(fields))
  colnames(data) <- unique(fields);
  as.data.frame(data)
}

edf.events <- function(EDFfile, type=c("ENDFIX"), fields=c("sttime","entime","eye","gavx",'gavy'))
{
  EDFfile <- path.expand((EDFfile))
  edf.events.c(EDFfile,type,fields)
}

edf.events.c <- function(EDFfile, type, fields)
{

  data <- .Call("get_events",EDFfile,type,unique(fields))
  colnames(data) <- unique(fields)
  as.data.frame(data)
}


edf.messages <- function(EDFfile)
{
  EDFfile <- path.expand((EDFfile))
  edf.messages.c(EDFfile)
}

edf.messages.c <- function(EDFfile)
{
  data <- .Call("get_messages", EDFfile)
  data <- data.frame(time=as.integer(data[,1]),
                     msg=as.character(data[,2]),
                     stringsAsFactors=F)
  # Entries with -1 aren't messages (i.e. events).  We filter them here and not
  # in the C++ part simply because it's easier to do in R.
  data[data$time!=-1,,drop=F]
}


