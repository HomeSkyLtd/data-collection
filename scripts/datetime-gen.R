
# Read from stdin the table
input <- read.table("stdin", header=TRUE)
if (is.null(input))
	stop("Invalid table")
# Get the timestamp column
times <- input$timestamp
if (is.null(times))
	stop("Couldn't find 'timestamp' column")

#Create POSIX lt 
posix <- as.POSIXlt(times, origin="1970-01-01")
#Get datetime variables
input$hour <- posix$hour
input$month_day <- (posix$mday - 1)
input$month <- posix$mon
input$week_day <- posix$wday
#Remove timestamp
input$timestamp <- NULL
#Write
write.table(input, quote=FALSE, row.names=FALSE)