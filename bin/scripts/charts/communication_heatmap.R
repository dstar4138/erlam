### 
### Chart: Communication HeatMap
### 
### Description:
###  Generates a heatmap of the number of yields per set number of
###  ticks per LPU. This will indicate the number of swap calls,
###  but not neccessarily the number of singular communications.
###
communication_heatmap <- function( dat ) {

    # Color palettes only have a max of 9 colors for heatmaps.
    # We therefore limit our tick range to a multiple of the max.
    HEAT_MAP_COLOR_MAX <- 9
  
    # Number of ticks to group together. 
    TICK_RANGE <- 2*HEAT_MAP_COLOR_MAX # Thus max "heat" is 18, min is 0
    
    # Filter by 'tick' and 'yield' events only.
    test <- dat[ ( dat$event=='tick' |
                   dat$event=='yield' ), ]

    # Construct matrix used for heat map:
    lpus <- count( list(test$lpu) )[[1]]
    freqs <- count( list(test[which(test$event=='tick'),]$lpu))$freq
    max_ticks <- max( freqs )
    nCols <- ceiling( max_ticks / TICK_RANGE )
    nRows <- length( lpus )
    density_m <- matrix( data=0, 
                         nrow=nRows, ncol=nCols )

    # For each lpu, calculate the heat for each tick range
    for(  ii in 1:nRows ) {
        lpu <- ii-1 # zero based LPU count
        event_stream <- test[ which(test$lpu == lpu), ]$event

        cur_tick <- 1
        tick_range_sum<-0 ; range_count<-0
        for( ei in 1:length(event_stream) ) {
        
            # If we find a yield in our range, up our sum,
            # otherwise, we're continuing along our range.
            if ( event_stream[ei]=='yield'  ) {
                tick_range_sum <- tick_range_sum + 1
            } else { # event == 'tick'
                range_count <- range_count + 1
            }

            if (range_count >= TICK_RANGE ) {
                density_m[ii,cur_tick] <- tick_range_sum
                tick_range_sum <- 0
                range_count <- 0
                cur_tick <- cur_tick+1
            }
        }
    
        if (cur_tick <= nCols ){
            density_m[ii,cur_tick] <- tick_range_sum
        }
    }
    
    # Export the plot.
    heatmap( density_m, 
             Rowv = NA, Colv = NA,
             col = brewer.pal(HEAT_MAP_COLOR_MAX,"Blues"),
             scale="none",
             xlab=paste("Communication Density per",TICK_RANGE,"Ticks"),
             ylab="Logical Processing Unit",
             main="LPU to Communication Density")
}

