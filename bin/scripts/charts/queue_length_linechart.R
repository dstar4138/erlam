### 
### Chart: Queue Length Line Chart
### 
### Description:
###  Generates a line chart of the size of each LPU's process queue over time.
###  As other heatmaps are based on averaging over a set of ticks, this
###  uses only the instances of reports and ignores lining up with the other 
###  charts.
###
queue_length_linechart <- function( dat ) {

    # Color palettes only have a max of 9 colors for heatmaps.
    # We therefore limit our tick range to a multiple of the max.
    HEAT_MAP_COLOR_MAX <- 9
  
    # Number of ticks to group together. 
    REPORT_RANGE <- 2*HEAT_MAP_COLOR_MAX # Thus max "heat" is 18, min is 0
    
    # Filter by 'queue_length'
    test <- dat[ which( dat$event=='queue_length' ), ]

    # If no queue_length reports, ignore further processing
    if( dim(test)[1] == 0 ) return()

    # Get information about the number of process queue's we'll be graphing
    tmp <- count( list(test$lpu) )
    lpus <- tmp[[1]]
    nLines <- length( lpus )

    # Construct the plot for the line chart
    xrange <- range(rescale(test$timestamp))
    yrange <- range(list(test$value))
    plot(xrange,yrange,
         xlab="Time", ylab="Process Queue Length", main="Max Queue Size")
    
    # Per Line attributes
    colors <- rainbow(nLines)
    linetype <- c(1:nLines)
    plotchar <- seq(18,18+nLines,1)
    
    # Finally for each lpu, add a line to the plot
    for( i in 1:nLines ) {
        lpu <- test[ which(test$lpu == lpus[i]), ]
        lines(rescale(lpu$timestamp), lpu$value, type="l", lwd=1.5,
              lty=linetype[i], col=colors[i], pch=plotchar[i]) 
    }
    
    # Make sure to label each line's LPU number
    legend(xrange[1], yrange[2], 1:nLines, cex=0.8, col=colors, 
           pch=plotchar, lty=linetype, title="LPU")
}

