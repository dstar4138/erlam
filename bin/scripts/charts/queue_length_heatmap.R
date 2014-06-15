### 
### Chart: Queue Length HeatMap
### 
### Description:
###  Generates a heatmap of the size of the queue's per LPU over time.
###  As other heatmaps are based on averaging over a set of ticks, this
###  uses only the instances of reports (and then finds the max of a subset).
###
queue_length_heatmap <- function( dat ) {

    # Color palettes only have a max of 9 colors for heatmaps.
    # We therefore limit our tick range to a multiple of the max.
    HEAT_MAP_COLOR_MAX <- 9
  
    # Number of ticks to group together. 
    REPORT_RANGE <- 2*HEAT_MAP_COLOR_MAX # Thus max "heat" is 18, min is 0
    
    # Filter by 'tick' and 'yield' events only.
    test <- dat[ ( dat$event=='queue_length' ), ]

    # Construct matrix used for heat map:
    tmp <- count( test$lpu )
    lpus <- tmp$x
    max_reports <- max( tmp$freq, 0 )
    nCols <- ceiling( max_reports / REPORT_RANGE )
    nRows <- length( lpus )
    density_m <- matrix( data=0, nrow=nRows, ncol=nCols )

    # For each lpu, calculate the heat for each report range
    for(  ii in 1:nRows ) {
        lpu <- ii-1 # zero based LPU count
        queue_stream <- test[ test$lpu == lpu, ]$value

        cur_tick <- 1; qi<-1; qsl <- length( queue_stream )
        while( qi < qsl ) {
            qie <- qi + REPORT_RANGE
            density_m[ii,cur_tick] <- max(queue_stream[ qi:qie ], 0, na.rm=TRUE)
            cur_tick <- cur_tick+1
            qi <- qie
        }
    }
    
    # Export the plot.
    density_m <- rescale(density_m, 0:HEAT_MAP_COLOR_MAX, 0:REPORT_RANGE, FALSE)
    heatmap( density_m, 
             Rowv = NA, 
             Colv = NA,
             col = brewer.pal(HEAT_MAP_COLOR_MAX,"Blues"),
             scale="none",
             xlab="Reports over time",
             ylab="Logical Processing Unit",
             main=paste("Max Queue Size per ",REPORT_RANGE," Reports"))
}

