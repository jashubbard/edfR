#' @title Determine trial number from an EDF file
#'
#' @description
#' \code{edf.trialcount} simply returns the number of trials in a given EDF file.
#'
#
#' @details
#' \code{edf.trialcount} may be useful if one wants to quickly get the number of trials
#' from an EDF with little overhead. Trials are defined by the starting and stopping of
#' recording from the eyetracker. To obtain trial-by-trial data from the EDF, use
#' \code{\link{edf.trials}}.
#'
#' @param EDFfile path to an EDF file
#'
#' @return The number of trials in the EDF
#'
#' @author Jason Hubbard, \email{hubbard3@@uoregon.edu}
#'
#' @examples
#' \dontrun{
#' output <- edf.trialcount('/path/to/file.edf')
#' output
#' 500
#' }
edf.trialcount <- function(EDFfile)
{
  EDFfile <- path.expand((EDFfile))
  count <- .Call("get_trial_count",EDFfile)
  count

}


#' @title Add an event mask to sample data
#'
#' @description
#' \code{eventmask} is an internal function that takes sample data from
#' an EDF file and adds binary variables indicating whether blinks, fixations,
#' or saccades occurred for each trial
#'
#' @details
#' eventmask is called internally from \code{\link{edf.samples}}, \code{\link{edf.trials}}, and \code{\link{edf.all}}
#' to add 3 binary variables to sample data: \code{fixation}, \code{saccade}, and \code{blink}. Each variable is coded
#' as 0 or 1 indicating the absence or presence of the fixation/saccade/blink for that sample. This is useful for quickly
#' indexing sample data based on events (for instance, removing pupil data during blinks). Eyelink (almost) always
#' records a saccade immediately before and after blinks, and those saccades are included as a part of the blink events.
#'
#' @param EDFfile path to an EDF file
#' @param samples a data frame of sample data, resulting from \code{\link{edf.samples}}
#'
#' @return A data frame of the same sample data, with the variables \code{fixation}, \code{saccade}, and \code{blink} added
#' to the end.
#'
#' @author Jason Hubbard, \email{hubbard3@@uoregon.edu}
#'
#' @examples
#' \dontrun{
#'
#' #This is called internally, but if you want to use it manually:
#' samples <- edf.samples('/path/to/file.edf')
#'
#' samples_with_mask <- eventmask('/path/to/file.edf',samples)
#'
#' #usually, you will use it in one of these ways:
#' trials <- edf.trials('/path/to/file.edf',samples=T,eventmask=T)
#' all <- edf.all('/path/to/file.edf',samples=T,eventmask=T)
#' samps <- edf.samples('/path/to/file.edf',eventmask=T)
#'
#' }
#'
eventmask <- function(EDFfile,samples=NULL)
{
  events <- edf.events(EDFfile,c('ENDBLINK','ENDSACC','ENDFIX'),c('sttime','entime','type'))

 # import all events, then break up into blinks (plus saccades), saccades only, and fixations only
  events <- events[order(events$entime),]
  sacc_and_blinks <- subset(events,type==4 | type==6)
  fixes <- subset(events,type==8)
  saccs <- subset(events,type==6)

  # eyelink records saccades immediately before and after blinks. Let's mark them as blinks
  isblink<- findRealBlinks(sacc_and_blinks$sttime,sacc_and_blinks$entime,sacc_and_blinks$type)


  # convert real blink events to samples
  blinksamp <- events2samples(isblink$sttime, isblink$entime, isblink$blink)
  colnames(blinksamp) <- c('time','sttime','entime','blink')
  blinksamp <- data.table::data.table(blinksamp,key='time') #data.tables are faster for merging
  blinksamp <- subset(blinksamp,select=c('time','blink'))


  # convert fixations to samples
  fixsamp <- events2samples(fixes$sttime, fixes$entime, fixes$type)
  colnames(fixsamp) <- c('time','sttime','entime','fixation')
  fixsamp <- data.table::data.table(fixsamp,key='time')
  fixsamp <- subset(fixsamp,select=c('time','fixation'))
  fixsamp[fixsamp$fix==8] <- 1


  # convert saccades to samples
  saccsamp <- events2samples(saccs$sttime, saccs$entime, saccs$type)
  colnames(saccsamp) <- c('time','sttime','entime','saccade')
  saccsamp <- data.table::data.table(saccsamp,key='time')
  saccsamp <- subset(saccsamp,select=c('time','saccade'))
  saccsamp[saccsamp$saccade==6] <- 1


  if(is.null(samples))
    samples <- edf.samples(f)

  samples <- data.table::data.table(samples,key='time')

  # merge everything together
  newsamp <- merge(samples,blinksamp,by='time',all.x=T)
  newsamp <- merge(newsamp,fixsamp,by='time',all.x=T)
  newsamp <- merge(newsamp,saccsamp,by='time',all.x=T)
  newsamp$blink[is.na(newsamp$blink)] <- 0
  newsamp$fixation[is.na(newsamp$fixation)] <- 0
  newsamp$saccade[is.na(newsamp$saccade)] <- 0

  return(data.frame(newsamp))


}

#' @title Create a fixation scatterplot from EDF
#'
#' @description
#' This creates a scatterplot of fixations read from an EDF file.
#'
#' @param EDFfile path to an EDF file. If you have already-loaded data, leave this as NULL
#' @param fixdata optionally you can provide a data frame of already imported fixation data
#' from \code{\link{edf.events}}. It expects the fields \code{gavx} and \code{gavy}
#' @param outfile if provided, it will save the plot as a PDF with this file  name
#' @param outlier.rm logical specifying whether to remove outlier fixations (>3SDs from mean)
#' @param res array specifying resolution of monitor. Only this region will be plotted.
#' @param flip logical indicating whether to flip the y axis so origin is in upper-left (defualt=TRUE).
#' @param theme either "black" or "white" specifying the plot theme (yellow on black or black on white)
#' @param crosshairs logical whether to plot crosshairs over the median x and y coordinates (dotted red lines)
#' @param plot.title title to put at the top of the plot
#'
#'
#' @author Jason Hubbard, \email{hubbard3@@uoregon.edu}
#'
#' @examples
#' \dontrun{
#'
#' edf.plot('/path/to/edffile.edf',outfile='~/Desktop/edfplot.pdf',res=c(1024,768))
#'
#' #just plot, don't save the PDF
#' edf.plot('/path/to/edffile.edf',res=c(1024,768),theme='white',plot.title='Fixations')
#'
#' }
#'
edf.plot <- function(EDFfile=NULL,fixdata=NULL,outfile=NULL,outlier.rm=F,res=NULL,flip=T,theme='black',crosshairs=T,plot.title='')
{

# if we give an EDF file, import and get data
if(!is.null(EDFfile) ){
  EDFfile <- path.expand((EDFfile))
  # get fixation data
  fixdata <- edf.events(EDFfile,type = c('ENDFIX'),fields = c('gavx','gavy'))
  # get just the edf file from the path
  heading <- basename(f)
}
  else
    heading=plot.title


x <- fixdata$gavx
y <- fixdata$gavy

# removing outliers (3.5 SDs from the mean)
if(outlier.rm){
  x.c <- scale(x,scale=T)
  y.c <- scale(y,scale=T)

  #find complete coordinates that are not outliers
  goodrows <- abs(x.c)<=3 & abs(y.c)<=3
  x <- x[goodrows]
  y <- y[goodrows]

  }


if(is.null(res)){
  # if no resolution given
  # round to nearest 50 pixels for the max x/y values
  xmax <- 50*round(max(x)/50)
  ymax <- 50*round(max(y)/50)

  xlim = c(0,xmax)
  ylim = c(0,ymax)}
else
    {
    # otherwise, zoom in to screen resolution
    xlim = c(0,res[1])
    ylim = c(0,res[2])
  }

  # flip y axis, since screen origin is at upper-left
  if(flip)
    ylim = rev(ylim)



  # if we're saving as a pdf
  if(!is.null(outfile))
  {
    # calculate width & height of PDF to preserve aspect ratio:
    # (original height / original width) x new width = new height
    w = 8 #let's make it 8 inches wide to fit on paper (it's vector graphics anyway)
    h = (max(ylim)/max(xlim))*w

    pdf(outfile,width=w,height=h)
  }

  # do we want yellow on black, or black on white?
  if(theme=='black')
    {bgcolor='black'
    pointcolor=adjustcolor('yellow',alpha.f=0.7)
    textcolor='white'
  }
  else if(theme=='white'){
    bgcolor='white'
    pointcolor=adjustcolor('black',alpha.f=0.7)
    textcolor='black'

  }

  # do the actual plotting
  par(bg=bgcolor)

  plot(x,y,
       xlim=xlim,
       ylim=ylim,
       pch=16,
       cex = 0.4,
       col = pointcolor,
       bg = pointcolor,
       type = 'p',
       axes = F,
       ann = F)

  title(main=heading,
        xlab='x coord (pixels)',
        ylab='y coord (pixels)',
        col.lab=textcolor,col.main=textcolor)

  xticks <- seq(0,xlim[2],100)

  if(flip)
    yticks <- seq(ylim[1],0,-100)
  else
    yticks <- seq(0,ylim[2],100)

  axis(1,at=xticks,labels=xticks,col=textcolor,col.axis=textcolor,las=2)
  axis(2, at=yticks,labels=yticks,col=textcolor,col.axis=textcolor,las=2)
  box(col=textcolor)

  # crosshairs on the median x/y limits
  if(crosshairs)
    abline(v=median(xlim),h=median(ylim),col=adjustcolor('red',alpha.f=1),lty=2)

  if(!is.null(outfile))
   dev.off()

}

#' @title Determine trial number from an EDF file
#'
#' @description
#' \code{edf.trialcount} simply returns the number of trials in a given EDF file.
#'
#
#' @details
#' \code{edf.trialcount} may be useful if one wants to quickly get the number of trials
#' from an EDF with little overhead. Trials are defined by the starting and stopping of
#' recording from the eyetracker. To obtain trial-by-trial data from the EDF, use
#' \code{\link{edf.trials}}.
#'
#' @param EDFfile path to an EDF file
#'
#' @return The number of trials in the EDF
#'
#' @author Jason Hubbard, \email{hubbard3@@uoregon.edu}
#'
#' @examples
#' \dontrun{
#' output <- edf.trialcount('/path/to/file.edf')
#' output
#' 500
#' }
edf.trialcount <- function(EDFfile)
{
  EDFfile <- path.expand((EDFfile))
  count <- .Call("get_trial_count",EDFfile)
  count

}


#' @title Batch process EDF files
#'
#' @description
#' \code{edf.batch} is useful for processing a bunch of EDF files at once and saving the resulting data
#'
#' @details
#' This function takes either a list of EDF file names, or a directory and search pattern for files, then
#' imports each one and (optionally) saves te data (either as .RData or .mat for matlab)
#'
#' @param EDFfiles a character vector of EDF files OR a directory where EDF files can be found
#' @param pattern a regular expression for matching file names of EDFs (e.g., '*.edf'). Only needed
#' if a directory is specified instead of a list of files.
#' @param samples a logical specifying whether samples should be included (default=FALSE)
#' @param do.plot a logical indicating whether to produce a plot for each EDF (uses \code{\link{edf.plot}})
#' @param save.plot a logical indicating whether a PDF of the plot should be saved (must specify \code{outdir})
#' @param save.files format for saving output files, either 'R' or 'matlab' (requires package \code{R.matlab}),
#' or set to \code{NA} if you do not want to save files (default=NA)
#' @param outdir a string specifying an output directory where to save the files. Required if \code{save.plot}
#' or \code{save.files} are used
#' @param plot.theme either "black" or "white" specifying the style of plot used in \code{\link{edf.plot}}
#' @param plot.res an array specifying the screen resoluion (e.g., c(1024, 768), used for \code{\link{edf.plot}}
#'
#'
#' @return a list of all trial data that was imported. WARNING: if you import samples and try to save this as 1
#' file it will be extremely large. It is better to use \code{save.files='R'} instead. All data will include a
#' column \code{ID} that correponds to only the numeric parts of the EDF file name. This is useful for concatenating all the data together
#'
#' @author Jason Hubbard, \email{hubbard3@@uoregon.edu}
#'
#' @examples
#' \dontrun{
#'
#' #import a list of named EDF files, don't save anything
#' #ID variable will be 1 for the first file, and 2 for the second
#' alldata <- edf.batch(c('/path/to/EDF1.edf',/path/to/EDF2.edf'),samples=F,do.plot=T,plot.theme='white',plot.res=c(1024,768)
#'
#'
#' #import all EDFs from basedir, and save all the output for R
#' basedir <- '~/Desktop/edfs'
#' alldata <- edf.batch(basedir,pattern='*.edf',samples=T,do.plot=T,save.plot=T,save.files='R',outdir='~/Desktop/output',plot.res=c(1024,768))
#'
#' #save for matlab, if you prefer (requires the package R.matlab):
#' alldata <- edf.batch(basedir,pattern='*.edf',samples=T,do.plot=T,save.plot=T,save.files='matlab',outdir='~/Desktop/output',plot.res=c(1024,768))
#'
#' }
#'
edf.batch <- function(EDFfiles=NULL,pattern=NULL,samples=FALSE,do.plot=TRUE,save.plot=FALSE,save.files=NA,outdir=NULL,plot.theme='black',plot.res = NULL)
{

  allt <- list()

  #if we give a directory and pattern, load all files from that directory
  if(!is.null(pattern) && length(EDFfiles)==1)
  {
   edfs <- list.files(path=EDFfiles,pattern=glob2rx(pattern))
   if(length(edfs)==0)
     stop('No EDFs found using the specified pattern')

    EDFfiles <- paste(EDFfiles,edfs,sep='')

  }

  for(f in seq_along(EDFfiles))
  {
    print(paste('Loading file:',path.expand(EDFfiles[[f]])))
    # get a clean name from the edf files
    justfile <- sub("^([^.]*).*", "\\1", basename(EDFfiles[[f]]))
    ID <- as.numeric(gsub("([0-9]*).*","\\1",justfile))

    #import
    trials <- edf.trials(EDFfiles[[f]],samples=samples,eventmask=T)

    #and ID and filename elements to the list
    trials$ID <- ID
    trials$filename <- EDFfiles[[f]]

    #add a (numeric only) ID column to each data frame
    trials$fixations$ID <- ID
    trials$saccades$ID <- ID
    trials$blinks$ID <- ID
    trials$messages$ID <- ID
    trials$header$ID <- ID

    #including samples if we imported them
    if(!is.null(trials$samples))
      trials$samples$ID <- ID

    #save into overall list
    allt[[f]] <- trials

    if(do.plot)
      {
      if(save.plot)
        outfile = file.path(outdir,paste(justfile,'.pdf',sep=''))
      else
        outfile=NULL

      edf.plot(fixdata=trials$fixations,theme=plot.theme,outfile = outfile,plot.title=justfile,res=plot.res)
    }

    if(save.files=='R' && !is.null(outdir))
    {
      outdata <- file.path(outdir,paste(justfile,'.RData',sep=''))
      print(sprintf('Writing file %s',outdata))
      save(trials,file=outdata);
    }
    else if(save.files=='matlab' && !is.null(outdir))
    {
      if (!requireNamespace("R.matlab", quietly = TRUE)) {
        stop("R.matlab package needed in order to save .mat files.",
             call. = FALSE)
      }

      outdata <- file.path(outdir,paste(justfile,'.mat',sep=''))
      print(sprintf('Writing file %s',outdata))

      #R.matlab saves everything as a matrix with no column names, so we save them separately
      trials$fixationnames <- t(names(trials$fixations))
      trials$saccadenames <- t(names(trials$saccades))
      trials$blinknames <- t(names(trials$blinks))
      trials$messagenames <- t(names(trials$messages))

      if(samples)
        trials$samplenames <- t(names(trials$samples))


      R.matlab::writeMat(outdata,eyedata=trials)

    }

    print(sprintf('Done with %s', justfile))

    }

  print("Done processing all files!")
  return(allt)
}


#' @title Epoch sample data
#'
#' @description
#' \code{epoch.samples} is used for time-locking sample data to a particular event and creating
#' separate epochs.
#'
#' @details
#' This is a basic function for epoching sample data (such as pupil diameter) relative to some time-locking event.
#' The event is indicated either as a message sent to the Eyelink during the experiment, or as a vector of times
#' of events (which must correspond to \code{time} in the sample data frame).
#'
#' @param samples a data frame of sample data obtained from \code{\link{edf.samples}}, \code{\link{edf.all}}, or
#'  \code{\link{edf.trials}}.
#' @param timelock either a string or pattern that matches a messsage sent to Eyelink (e.g., 'STIMONSET', 'TRIAL [0-9]*'),
#' or a vector of times for events of interest.
#' @param sample.field a character corresponding to the sample field that you are epoching (e.g., 'paL' or 'paR')
#' @param epoch the starting and ending samples of the epoch, relative to the event (decault = c(-100, 100))
#' @param messages a data frame obtained from \code{\link{edf.messages}}, \code{\link{edf.all}}, or \code{\link{edf.trials}}
#' which contains the messages sent to Eyelink. Only necessary if \code{timelock} is a character
#' @param eyetrial logical indicating whether to include the eyetrial resulting from \code{\link{edf.trials}}. This will be
#' included as part of the messages data frame if you use \code{\link{edf.trials}}. Default = FALSE
#'
#' @return a list with fields:
#' \itemize{
#' \item \code{timelock} a numeric vector of the time-locking events
#' \item \code{epoch.window} a vector indicating the epoch times you specified with \code{epoch}
#' \item \code{sample.field} a character indicating the sample field you specified with \code{sample.field}
#' \item \code{message} if a message is provided, a character vector indicating the message for each epoch
#' \item \code{eyetrial} if eyetrial=TRUE, then the corresponding eyetrial for each epoch
#' \item \code{epochs} a matrix of the actual epoched data (events x epoch_width)
#' }
#'
#' @author Jason Hubbard, \email{hubbard3@@uoregon.edu}
#'
#' @examples
#' \dontrun{
#'
#' #the easiest way is to use edf.trials:
#' trials <- edf.trials('/path/to/EDFfile.edf',samples=T,eventmask=T)
#' trials$samples$paR[trials$samples$blink==1] <- NA #quick blink removal, not perfect
#' epochdata <- epoch.samples('STIMONSET',trials$samples,sample.field='paR',
#'                            epoch=c(-500,500),messages=trials$messages,eyetrial=T)
#'  #plot it!
#'  matplot(t(epochdata$epochs[1:20,]),type='l')
#'
#' #if you want to timelock to some type of event:
#' events <- edf.events('/path/to/EDFfile.edf',type='STARTSACC',fields='sttime')
#' samples <- edf.samples('/path/to/EDFfile.edf')
#' epochdata <- epoch.samples(events$sttime,samples,sample.field='paR',epoch=c(-500,1))
#'
#'
#' }
#'
epoch.samples <- function(timelock,samples,sample.field='paL',epoch = c(-100,100),messages=NULL,eyetrial=FALSE)
{

  result = list()
  result$timelock <- timelock
  result$epoch.window <- epoch
  result$sample.field <- sample.field
  result$message <- NULL
  result$eyetrial <- NULL

  # we can either give a vector of times, or a string that matches an event
  if(is.character(timelock) && !is.null(messages))
  {
    #find the time-locking message
    #expects messages to come from output of edf.messages or edf.trials
    findmsg <- grepl(timelock,messages$message)
    result$message <- messages$message[findmsg] #save the actual messages so we can double-check later

    if(length(unique(result$message))>1)
      warning('More than 1 unique time locking event found. Make sure this is what you want');

    result$timelock <- messages$sttime[findmsg]

    if(eyetrial)
      result$eyetrial <- messages$eyetrial[findmsg]
  }

  result$epochs <- get_epochs(result$timelock,samples$time,samples[,sample.field],epoch[1],epoch[2])

  return(result)
}

#' @title Combine the results from edf.batch
#'
#' @description
#' \code{combine.eyedata} can be used to take the results from \code{\link{edf.batch}} and stack
#' into large data frames for fixations, saccades, etc.
#'
#' @details
#' \code{edf.batch} returns a list where each element has fields \code{fixations}, \code{saccades},
#' \code{blinks}, and (optionaly) \code{samples}. This function takes that result and creates a single
#' data frame for all subjects' eye data
#'
#' @param batchdata output from \code{\link{edf.batch}}
#' @param fields character array of the particular fields you want to combine (default is c('fixations',
#' 'saccades','blinks','messages')). Warning: combining samples will result in extremely large data frames (millions of rows per subject)
#'
#' @return a list with each of the requested fields. Each one is a data frame with all subjects' data. Subjects will
#' be identified by the "ID" varaiable.
#'
#' @author Jason Hubbard, \email{hubbard3@@uoregon.edu}
#'
#' @examples
#' \dontrun{
#'
#' #first load everything:
#' allbatch <- edf.batch('~/Desktop/edfs','*.edf')
#'
#' alldata <- combine.eyedata(allbatch)
#'
#' all_fixations <- alldata$fixations
#' all_saccades <- alldata$saccades
#'
#' }
#'
combine.eyedata <- function(batchdata,fields=c('fixations','saccades','blinks','messages','header'))
{
  output <- list()

 for(f in fields){
   tmp <- sapply(batchdata,function(x) x[[f]],simplify = F)
   tmp <- do.call(rbind,tmp)

   output[[f]] <- tmp
   }

  return(output)

}

