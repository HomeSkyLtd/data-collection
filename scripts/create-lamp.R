#CREATE-LAMP
args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1 || length(args) > 3)
	stop("Usage: Rscript create-lamp.R MIN_LIGHT [START_HOUR [END_HOUR]]")

#Get input
min_light <- as.numeric(args[1])
input <- read.table("stdin", header=TRUE)
lamp <- (input$light >= min_light)

if (length(args) > 1) {
	min_hour <- as.integer(args[2])
	max_hour <- 23
	if (length(args) == 3) {
		max_hour <- as.integer(args[3])
	}

	if (min_hour < max_hour)
		lamp <- lamp & (input$hour >= min_hour & input$hour <= max_hour)
	else
		lamp <- lamp & (input$hour >= min_hour | input$hour <= max_hour)

}

input$lamp <- as.integer(lamp)
write.table(input, quote=FALSE, row.names=FALSE)