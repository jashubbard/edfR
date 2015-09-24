# this code is heavily based on code from Titus von der Malsburg: http://pages.ucsd.edu/~tvondermalsburg/
edf.preamble <- function(EDFfile)
{
	.Call("get_preamble",EDFfile)
}


edf.all <- function(EDFfile,samples=FALSE)
{
#this loads all the most commmon data. Samples are turned off by default
  #separates into fixations, saccades, and blinks
  data <- list()

  EDFfile <- path.expand((EDFfile))

  if(samples){

    samp <- edf.samples(EDFfile)
    data$samples <- samp
  }


  events <- edf.events(EDFfile,type=c("ENDFIX",'ENDSACC','ENDBLINK'),fields=c('type','sttime','entime','gavx','gavy','gstx','gsty','genx','geny','avel','pvel','message'))

  data$blinks <- subset(events,type==4)[c('sttime','entime')]
  data$saccades <- subset(events,type==6)[c('sttime','entime','gstx','gsty','genx','geny','avel','pvel')]
  data$fixations <- subset(events,type==8)[c('sttime','entime','gavx','gavy')]


  messages <- edf.messages(EDFfile)

  data$messages <- messages

  data

}

edf.trialcount <- function(EDFfile)
{
  EDFfile <- path.expand((EDFfile))
  count <- .Call("get_trial_count",EDFfile)
  count

}


edf.trials <- function(EDFfile,samples=F)
{
  #getting trial-by-trial data (useful for merging with behavioral data)
  EDFfile <- path.expand((EDFfile))
  eventfields <- c("time", "type", "read", "eye", "sttime", "entime", "hstx", "hsty", "gstx", "gsty", "sta",
    "henx", "heny", "genx", "geny", "ena", "havx", "havy", "gavx", "gavy", "ava", "avel",
    "pvel", "svel", "evel", "supd_x", "eupd_x", "supd_y", "eupd_y", "status", "flags",
    "input", "buttons", "parsedby");

  samplefields <- c("time", "gxL","gyL","paL","gxR","gyR","paR")

  output = list()

  data <- .Call("get_trial_id",EDFfile,eventfields,samplefields,as.numeric(samples))
  trialtimes <- data[[1]]
  trialtimes <- apply(trialtimes,2,as.numeric)
  trialtimes <- data.frame(trialtimes)
  names(trialtimes) <- c('eyetrial','starttime','endtime','duration')

  events <- as.data.frame(do.call(rbind,data[[2]]))
  names(events) <- c(eventfields,'eyetrial')

  fixes <- subset(events,type==8)[,c('eyetrial','sttime','entime','gavx','gavy')]
  saccs <- subset(events,type==6)[,c('eyetrial','sttime','entime','gstx','gsty','genx','geny','avel','pvel')]
  blinks <- subset(events,type==4)[,c('eyetrial','sttime','entime')]


  messages <- data[[3]]
  messages <- lapply(messages,as.character)
  messages <- unlist(messages)
  messages[messages=='NULL'] <- NA
  events$message <- messages


  if(samples)
  {
    samples <-as.data.frame(do.call(rbind,data[[4]]))
    names(samples) <- c(samplefields,'eyetrial')
    samples <- samples[c('eyetrial',samplefields)] #make trial the first variable
  }
else
  samples <- NULL


  output$header <- trialtimes
  output$messages <- events[!is.na(events$message),c('eyetrial','sttime','message')]
  output$samples <- samples
  output$fixations <- fixes
  output$saccades <- saccs
  output$blinks <- blinks



#   data <-do.call(rbind,data)
#   data <- as.data.frame(data)
#   names(data) <- eventfields
  output

}

edf.samples <- function(EDFfile, fields=c("time","flags","gxL","gyL","paL","gxR","gyR","paR"))
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

	data <- edf.samples.c(EDFfile, fields)
	data <- as.data.frame(data)
	colnames(data) <- fields
	data$time <- as.integer(data$time)
	data
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
