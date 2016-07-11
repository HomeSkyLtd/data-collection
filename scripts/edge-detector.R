#EDGE-DETECTOR

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1)
    stop("Usage: Rscript edge-detector.R VARIABLE")

variable <- args[1]
input <- read.table("stdin", header=TRUE)
