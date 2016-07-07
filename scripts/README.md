## R scripts
This folder contains some R scripts to clean the data. A script perform a specific task, so to clean your data you'll need to run some of them.
All scripts read from stdint and write to stdout.
###Scripts

####datetime-gen.R
Usage: Rscript datetime-gen.R
This script assumes that there is a column called timestamp, which is a Unix timestamp and from it creates the following columns:
- hour
- month_day
- month
- week_day (0 for sunday and 6 for saturday)
It removes the timestamp column, adds these 4 new column and sends the new table to stdout

####lowpass-filter.R
Usage: Rscript lowpass-filter.R VARIABLE WINDOW_SIZE
Pass a lowpass filter in the input variable.

####edge-detector.R
Usage: Rscript edge-detector.R VARIABLE
This script remove all lines that are not before an edge (or change) in the selected variable.

