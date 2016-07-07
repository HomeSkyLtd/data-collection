#LOWPASS
# Read args
args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 2)
	stop("Usage: Rscript lowpass.R VARIABLE WINDOW_SIZE")

#Get input
variable <- args[1]
window_size <- as.integer(args[2])
input <- read.table("stdin", header=TRUE)

#Check input
if (is.null(input))
	stop("Invalid table")
if (!(variable %in% names(input)))
	stop("Table doesn't contain column '", variable, "'")
if (window_size <= 0)
	stop("Window size must be bigger than 0")

#Process
new_data <- input[variable]
#Keep track of the previous 1
start_one = -1
for (i in 1:(nrow(new_data))) {
	if (new_data[i,1] == 1) {
		if (start_one != -1 && (i - start_one) <= window_size) {
			new_data[start_one:i,1] = 1
		}
		start_one = i
	}
}

input[,variable] = new_data[,1]
write.table(input, quote=FALSE, row.names=FALSE)