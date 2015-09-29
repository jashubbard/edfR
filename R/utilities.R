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
  isblink<- findRealBlinks(events$sttime,events$entime,events$type)


  # convert real blink events to samples
  blinksamp <- events2samples(isblink$sttime, isblink$entime, isblink$blink)
  colnames(blinksamp) <- c('time','sttime','entime','blink')
  blinksamp <- data.table(blinksamp,key='time') #data.tables are faster for merging
  blinksamp <- subset(blinksamp,select=c('time','blink'))


  # convert fixations to samples
  fixsamp <- events2samples(fixes$sttime, fixes$entime, fixes$type)
  colnames(fixsamp) <- c('time','sttime','entime','fixation')
  fixsamp <- data.table(fixsamp,key='time')
  fixsamp <- subset(fixsamp,select=c('time','fixation'))
  fixsamp[fixsamp$fix==8] <- 1


  # convert saccades to samples
  saccsamp <- events2samples(saccs$sttime, saccs$entime, saccs$type)
  colnames(saccsamp) <- c('time','sttime','entime','saccade')
  saccsamp <- data.table(saccsamp,key='time')
  saccsamp <- subset(saccsamp,select=c('time','saccade'))
  saccsamp[saccsamp$saccade==6] <- 1


  if(is.null(samples))
    samples <- edf.samples(f)

  samples <- data.table(samples,key='time')

  # merge everything together
  newsamp <- merge(samples,blinksamp,by='time',all.x=T)
  newsamp <- merge(newsamp,fixsamp,by='time',all.x=T)
  newsamp <- merge(newsamp,saccsamp,by='time',all.x=T)
  newsamp$blink[is.na(newsamp$blink)] <- 0
  newsamp$fixation[is.na(newsamp$fixation)] <- 0
  newsamp$saccade[is.na(newsamp$saccade)] <- 0

  return(data.frame(newsamp))


}

edf.plot <- function(EDFfile,outlier.rm=F,res=NULL,flip=T)
{
EDFfile <- path.expand((EDFfile))

fixdata <- edf.events(EDFfile,type = c('ENDFIX'),fields = c('gavx','gavy'))

x <- fixdata$gavx
y <- fixdata$gavy

# removing outliers (3.5 SDs from the mean)
if(outlier.rm){
  x.c <- scale(x,scale=T)
  y.c <- scale(y,scale=T)

  #x outliers
  x <- x[abs(x.c)<=3]
  y <- y[abs(x.c)<=3]

  #y outliers
  x <- x[abs(y.c)<=3.5]
  y <- y[abs(y.c)<=3.5]
  }

  if(is.null(res)){
   xlim = c(0,max(x))
   ylim = c(0,max(y))}
  else
    {
    xlim = c(0,res[1])
    ylim = c(0,res[2])
  }

  if(flip)
    ylim = rev(ylim)

  splitf <- strsplit(EDFfile,.Platform$file.sep)
  splitf <- splitf[[1]][length(splitf[[1]])]

  par(bg='black')

  plot(x,y,
       xlim=xlim,ylim=ylim,
       pch=20,
       cex = 0.4,
       col = adjustcolor('yellow',alpha.f=0.5))

  title(main=splitf,
        xlab='x coord (pixels)',
        ylab='y coord (pixels)',
        col.lab='white',col.main='white')
  xticks <- seq(0,xlim[2],100)
  yticks <- seq(0,ylim[2],100)

  axis(1,at=xticks,labels=xticks,col='white',col.axis='white',las=2)
  axis(2, at=yticks,labels=yticks,col='white',col.axis="white",las=2)
  box(col="white")
  abline(v=median(xlim),h=median(ylim),col=adjustcolor('red',alpha.f=1),lty=2)

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


#' @title Epoch sample data
#'
#' @description
#' \code{epoch.samples} is used for time-locking sample data to a particular event and creating
#' separate epochs.
#'
#' @details
#' This is a basic function for epoching sample data (such as pupil diameter) relative to some time-locking event.
#' The event must be indicated by a message sent to Eyelink. The function will look for the field \code{sttime} to
#' get the timing for each epoch.
#'
#' @param samples a data frame of sample data obtained from \code{\link{edf.samples}}, \code{\link{edf.all}}, or
#'  \code{\link{edf.trials}}. If missing you can optionally supply EDFfile='path/to/file.edf' to load in a file
#' @param messages a data frame of sample data obtained from \code{\link{edf.messages}}, \code{\link{edf.all}},
#' or \code{\link{edf.trials}}. If missing you can optionally supply EDFfile='path/to/file.edf' to load in a file
#' @param event a string corresponding to the sample data to be epoched (default = \code{'paL'})
#' @param epoch the starting and ending samples of the epoch, relative to the event (decault = c(-100, 100))
#' @param format a string (either "wide" or "long") indicating whether epoched data should returned in long format
#' (a data frame with the samples and the epoch number) or wide format (a matrix with rows corresponding to epoch numbers)
#' (default = 'wide')
#' @param EDFfile (optional) instead of supplying samples and messages, and EDF file can be given
#'
#' @return Either a data frame with the sample data plus an "epoch" variable (long format) or a matrix with rows corresponding
#' to epochs and columns corresponding to time (wide format).
#'
#' @author Jason Hubbard, \email{hubbard3@@uoregon.edu}
#'
#' @examples
#' \dontrun{
#'
#' #This is called internally, but if you want to use it manually:
#' samples <- edf.samples('/path/to/file.edf')
#' messages <- edf.messages('/path/to/file.edf')
#'
#' epoch.mat <- epoch.samples(samples,messages,'STIMONSET',epoch=c(-500,100),field='paL',format='wide')
#' epoch.df <- epoch.samples(samples,messages,'STIMONSET',epoch=c(-500,100),field='paL',format='long')
#' }
#'
epoch.samples<- function(samples=NULL,messages=NULL,event,epoch=c(-100,100),field='paL',format='wide',EDFfile=NULL)
{

if(is.null(samples))
{
 allt <- edf.trials(EDFfile,samples=T,eventmask = T)
 samples <- allt$samples
 messages <- allt$messages

}

epoch_width=(epoch[2] - epoch[1])

timelock <- messages$sttime[grepl(event,messages$message)]

epoch_st <- timelock+epoch[1]
epoch_en <- timelock+epoch[2]-1
epoch_num <- 1:length(epoch_st)

if(any((epoch_st - lag(epoch_en,1))<0,na.rm=T))
  stop('At least some epochs are overlapping. Try a smaller epoch')

# expand our message intervals into samples
tmp <- as.data.frame(events2samples(epoch_st,epoch_en,epoch_num))
names(tmp) <- c('time','st','en','epoch')
tmp <- subset(tmp,select=c('time','epoch'))

# then merge with our sample data
samples <- data.table(samples)
samples <- setkey(samples,'time')
tmp2 <- merge(samples,tmp,all.y=T)
tmp2 <- subset(tmp2,!is.na(epoch)) #throw out data not occurring in an epoch

if(format=='wide')
{
# create a list with epoched data, convert to matrices
tmp2 <- split(tmp2[,field],tmp2$epoch)
tmp2 <- lapply(tmp2,function(x) matrix(x,nrow=1,ncol=epoch_width))

# then stack them into 1 big matrix
tmp3 <- do.call(rbind,tmp2)
}
else if(format=='long') {
  tmp3 <- tmp2 #if we want long format, then just return what we have
}

return(tmp3)

}
