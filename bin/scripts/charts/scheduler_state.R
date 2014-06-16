###
### Chart: Scheduler State Over Time
###
### Description:
###   Generates a pseudo-heatmap to represent the state of 
###   a LPU over time. There are 4 possible states a scheduler
###   can claim to be in: startup, running, waiting, and 
###   stopping.
###
### Parameters:
###   TICK_RANGE - Used to average over a standardized
###     number of ticks. However, we use the average tick
###     size times the range to get a step size.
###
scheduler_state = function( dat, TICK_RANGE=18 ) {
  
  # Filter the data-table to just show the scheduler state 
  # report. This report should happen on a fairly average 
  # basis (i.e. at almost every tick).
  test <- dat[ ( dat$event=='sched_state' ), ]
  
  # If there are no state events, then return
  if (dim(test)[1] == 0) return()
  
  # Our heatmap will consist of only four values:
  WAITING <- 1
  STARTUP <- 2
  RUNNING <- 3
  STOPPED <- 4
  
  # Find our average time-range between ticks to know our
  # step length. Then find our number of columns and rows
  # for our plot.
  first_lpu_ticks <- dat[ (dat$lpu == 0 & dat$event == 'tick'), ]
  step_length <- (first_lpu_ticks[2,]$timestamp -
                    first_lpu_ticks[1,]$timestamp ) * TICK_RANGE
  time_range <- range( list(test$timestamp) )
  time_steps <- seq(from=time_range[1], 
                    to=time_range[2], 
                    by=step_length)
  
  # Build our pseudo-heatmap matrix at the correct size
  channels_range <- count(list(test$lpu))[1]
  nCols <- length( time_steps )
  nRows <- dim( channels_range )[1]
  mat <- matrix( data=STARTUP, 
                 nrow=nRows, ncol=nCols )
  
  # We will keep track of the current state over time to
  # plug it into our matrix
  cur_state <- rep.int(STARTUP, nRows)
  cur_time <- 1 ; prev_time <- 1
  
  for( t in time_steps ) { # For each timestamp subsection
    events <- test[ which(test$timestamp > prev_time &
                          test$timestamp <= t), ]
    levents <- dim(events)[1]
    
    # For each lpu event, update the sched state.
    for( cur_event in 1:levents ){
        channelId <- events[cur_event,]$lpu + 1 #one-indexed aray
        channelEvent <- paste(events[cur_event,]$value, "", sep="")
        if( channelEvent == 'startup' ) {
          cur_state[ channelId ] <- STARTUP
        } else if( channelEvent == 'waiting') {
          cur_state[ channelId ] <- WAITING
        } else if( channelEvent == 'running') {
          cur_state[ channelId ] <- RUNNING
        } else if( channelEvent == 'stopping') {
          cur_state[ channelId ] <- STOPPED
        }
    }
    
    # For each channel, set the tick equal to the current 
    # state of the channel.
    for( c in 1:nRows ) { mat[c,cur_time] <- cur_state[c] }
    cur_time <- cur_time+1
    prev_time <- t
  }
  
  # Export the Heatmap. 
  xlabel <- "Time (ticks)" ; ylabel <- "Scheduler (id)"
  mainlab <- "Scheduler State Over Time"
  colors <- brewer.pal(4,"Accent")
  
  # However, if there has only been one channel we need to 
  # resort to the image() function (due to R being qwerky).
  if( nRows < 2 | nCols < 2 ) {
    ylabel<-paste(ylabel,"\n1",sep="")
    image(   t(mat),
             col=colors,
             xaxt='n',yaxt='n',mgp=c(0,1,0),
             xlab=xlabel,ylab=ylabel,
             main=mainlab )
  } else {
    heatmap( mat,
             Rowv=NA, Colv=NA,
             col=colors,
             scale="none",
             xlab=xlabel,ylab=ylabel,
             main=mainlab )
  }
  
  #TODO: Add a legend to explain state colors.
}