###
### Chart: Channel Usage Over Time
###
### Description:
###   Generates a pseudo-heatmap to represent the usage of 
###   a channel over time. Dark regions mean it is blocked, 
###   where as light regions show where it is unblocked;
###   although we ignore by whom.
###
### Parameters:
###   TICK_RANGE - Used to average over a standardized
###     number of ticks. However, we use the average tick
###     size times the range to get a step size.
###
channel_usage = function( dat, TICK_RANGE=18 ) {
  
  # Filter the data-table to just show channel_blocked and
  # channel_unblocked events. Of these, we only care about
  # two columns: timestamp and lpu (which in this case is
  # actually representing the channel ID).
  test <- dat[ ( dat$event=='channel_blocked' | 
                 dat$event=='channel_unblocked' ), ]
  
  # If there are no channel events (no communication), then
  # there is no use building a channel usage chart.
  if (dim(test)[1] == 0) return()
  
  # Our heatmap will consist of only two values:
  UNBLOCKED <- 0
  BLOCKED <- 3 # brewer.pal() requests minimum colours > 2.
  
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
  mat <- matrix( data=UNBLOCKED, 
                 nrow=nRows, ncol=nCols )
  
  # We will keep track of the current state over time to
  # plug it into our matrix
  cur_state <- rep.int(UNBLOCKED, nRows)
  cur_time <- 1 ; prev_time <- 1
  
  for( t in time_steps ) { # For each timestamp subsection
    events <- test[ which(test$timestamp > prev_time &
                          test$timestamp <= t), ]
    levents <- dim(events)[1]
    
    # For each channel event, update the state.
    for( cur_event in 1:levents ){
        channelId <- events[cur_event,]$lpu + 1 #one-indexed aray
        channelEvent <- paste(events[cur_event,]$event, "", sep="")
        if( channelEvent == 'channel_blocked' ) {
          cur_state[ channelId ] <- BLOCKED
        } else if( channelEvent == 'channel_unblocked') {
          cur_state[ channelId ] <- UNBLOCKED
        }
    }
    
    # For each channel, set the tick equal to the current 
    # state of the channel.
    for( c in 1:nRows ) { mat[c,cur_time] <- cur_state[c] }
    cur_time <- cur_time+1
    prev_time <- t
  }
  
  # Export the Heatmap. 
  xlabel <- "Time (ticks)" ; ylabel <- "Channel (id)"
  mainlab <- "Channel State Over Time"
  colors <- brewer.pal(BLOCKED,"Greys")
  
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