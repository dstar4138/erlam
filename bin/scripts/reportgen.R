###
### ErLam Log Report Generation:
###
### Usage: Rscript loggen.R <program.erlamlog>
### 
### Description:
### This script is used by ErLam to generate visual reports after the execution
### of a compiled script. Please see the runtime documentation for it's usage.
###
### Package Requirements:
Required <- c( 
              "ggplot2",
              "reshape2", 
              "RColorBrewer"
            )

# Get command-line option of path to erlamlog.
filename <- commandArgs(TRUE)[1]
if (!file.exists( filename )) {
    print("ERROR: ErLam log non-existent.")
    quit("no",1)  #dont save progress, statuscode=1
}

# Load required libraries or fail
rets <- sapply( Required, library, logical.return=TRUE,
                                   character.only=TRUE )
if( !(sum(rets) == length(Required)) ){
    print("ERROR: Unable to load required library.")
    quit("no", 1)
}

## Update system options ##
options( digits=12 ) # Timestamp is ssssss.mmmmmm (12 digits max)

# Open the (possibly very large) csv log file.
dat <- read.csv( filename )

# Sort the data matrix by the timestamp
dat <- dat[ order(dat$timestamp), ]

# Set up report document location next to log
report_name <- paste(filename, "-report.pdf", sep="")
pdf(file=report_name)

# Get list of charts needed by scraping sub directory.
for( chartf in list.files("./charts")){ 
    # Load that charts file:
    source( paste("charts/",chartf,sep="") )

    # Get the name of the function:
    chartfun <- sub("\\.R", "", chartf)

    # Check to make sure it exists after sourcing:
    if( exists( chartfun, mode="function" ) ){

        # Get the function and run it with the sorted data matrix:
        tmp <- get( chartfun )
        try( tmp( dat ) )
    }
}

