library("dplyr", quietly = TRUE, warn.conflicts = FALSE)

#
# I/O FUNCTIONS
#

# Reads Snapshot
#   from is the file containing the Snapshot. If it is missing, it will read from stdin
#   Returns the read table
snap.read <- function (from) {
    if (missing(from))
        from <- "stdin"
    tbl_df(read.table(from, header=TRUE))
}

# Saves Snapshot
#   table is the tbl_df to save
#   to is the file to save the snapshot. If it is missing, send to stdout
snap.save <- function (table, to) {
    if (missing(to))
        to <- ""
    write.table(x = table, file = to, quote = FALSE, row.names = FALSE)
}


#
# GENERIC CLEANING FUNCTIONS
#

# Smooth presence data, taking the mean value of the presence in a moment and
# n-1 subsequent values
#   table is the tbl_df to process
#   column is the column to smooth
#   n is the window size to calculate the mean
snap.clean.smooth <- function(table, column, n) {
    nrows <- length(table[column][[1]])
    for (i in 1:nrows) {
        table[i, column] <- mean(table[i:min(c(i+n-1, nrows)), column][[1]])
    }
    table
}

# DEPRECATED: Binary lowpass
# Pass a lowpass filter in the column. If between two 'stable level' values there are at most 'window size' rows, all
# values in that window become 'stable level'
#   table is the tbl_df to clean
#   column is the column to apply the lowpass
#   window_size is the max size of the lowpass window
#   stable_level is the stable level of the column (0 or 1). The default is 1.
#   Returns the new table
snap.clean.binary_lowpass <- function (table, column, window_size, stable_level) {
    if (missing(stable_level))
        stable_level <- 1
    new_data <- table[column]
    #Keep track of the previous stable level
    start_level = -1
    for (i in 1:(nrow(new_data))) {
        if (new_data[i, 1] == stable_level) {
            if (start_level != -1 && (i - start_level) <= window_size) {
                new_data[start_level:i,1] = stable_level
            }
            start_level = i
        }
    }
    temp_tb <- tbl_df(tb)
    temp_tb[column] <- new_data
    temp_tb
}



#
# GENERIC EXTRACT DATA FUNCTIONS
#

# Cleans timestamp column (kind of generic because all datasets will have)
# Generates hour (0-23), month_day (1-31), month(1-12), week_day (0-6 starting at sunday) and removes timestamp
#   table is the tbl_df to clean
#   Returns the new table
snap.extract.timestamp <- function (table) {
    posix <- as.POSIXlt(select(table,timestamp) %>% unlist(), origin="1970-01-01")
    #select(mutate(table, hour = posix$hour, week_day = posix$wday,
    #              month = posix$mon + 1, month_day = posix$mday), -timestamp)
    #For now, just use hour and week_day
    select(mutate(table, hour = posix$hour, week_day = posix$wday), -timestamp)
}


# Get column edges
#   Get the value before the edge and add a column (changed_to) indicating the produced value
#   table is the tbl_df to clean
#   column is the column that the edges must be examinatde
#   Returns the new table
snap.extract.edges <- function (table, column) {
    shifted_column <- append(table[2:nrow(table), column][[1]], 0)
    temp_tb <- tbl_df(table)
    new_column <- temp_tb[column][[1]] != shifted_column
    temp_tb <- mutate(table, actuate = new_column)
    temp_tb
}



# 
# LIGHTNING FUNCTIONS
#

# Add Lamp column
# Ensure that your table has light and hour variables
#   table is the tbl_df to clean
#   min_light is the min value of the light column in which the lamp is on
#   start_hour is the start of the window when the light can be on (default is 0)
#   end_hour is the end of the window when the light can be on (default is 23)
#   Returns the new table
snap.light.add_lamp <- function (table, min_light, start_hour, end_hour) {
    if (missing(start_hour))
        start_hour <- 0
    if (missing(end_hour))
        end_hour <- 23
    if (start_hour < end_hour)
        mutate(table, lamp = as.integer((light >= min_light) & (hour >= start_hour & hour <= end_hour)))
    else
        mutate(table, lamp = as.integer((light >= min_light) & (hour >= start_hour | hour <= end_hour)))
}


# Do default cleaning on light
snap.light.default <- function (table, min_light = 20, start_hour = 18, end_hour = 3) {
    snap.extract.timestamp(table)  %>% 
    snap.light.add_lamp(min_light, start_hour, end_hour)  %>% 
    snap.extract.edges('lamp')
}

# Cleans the data, keeping only the variations in data represented by column
# Removes column data afterwards, keeping only the future variation
snap.extract.keep.edges.only <- function(table, column) {
  varval <- lazyeval::interp(~(column+1) %% 2, column=as.name(column))
  
  smoothed_edge <- table %>%
    snapshot.clean.get_edges(column) %>%
    filter(actuate==TRUE) %>%
    mutate_(.dots=setNames(list(varval), "action")) %>%
    select_(.dots=c("-actuate", paste("-", column, sep="")))
}


# Does standard cleaning: process timestamps, smoothes presence and keeps only edges
# in column.
#   n is the number of entries used to smooth presence
#   column is the column that will be analyzed for edges
snap.light.batch.edges.only <- function(table, n, column) {
  table <- table %>%
    snap.extract.timestamp() %>%
    snap.clean.smooth('presence', n) %>%
    snap.extract.keep.edges.only(column)
}