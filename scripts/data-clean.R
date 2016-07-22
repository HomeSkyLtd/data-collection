library("dplyr", quietly = TRUE, warn.conflicts = FALSE)
library(DMwR)

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
snap.clean.smooth.subsequent <- function(table, column, n) {
    nrows <- length(table[column][[1]])
    for (i in 1:nrows) {
        table[i, column] <- mean(table[i:min(c(i+n-1, nrows)), column][[1]])
    }
    table
}

snap.clean.smooth.precedent <- function(table, column, n) {
  nrows <- length(table[column][[1]])
  for (i in nrows:1) {
    table[i, column] <- mean(table[max(i-n+1, 1):i, column][[1]])
  }
  table
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


#Tells which action was taken. Creates a new column with this information.
#   1 => light_on from 0 to 1
#   0 => light_on constant
#   -1=> light_on from 1 to 0
snap.extract.action <- function(table, column) {

    nrows <- length(table[column][[1]]);
    
    after <- table[column][2:nrows, 1];
    before <- table[column][1:(nrows - 1), 1];
    
    table["action"] <- rbind(after - before, c(0));
    
    table
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
    snap.clean.smooth('presence', 5) %>%
    snap.extract.edges('lamp') 
}

# Try 2: keeps track of conditions when no action occurs
# This results in highly unbalanced classes, so apply SMOTE to balance them
# Also, smoothes presence data using average with n subsequent samples 
snap.clean.batch.balance <- function(table, n, edge_column) {
  table <- snap.light.action(input, edge_column)
  table <- table %>%
    select(c(light, presence, action)) %>%
    snap.clean.smooth.subsequent('presence', n)
  light_max <- max(table$light)
  table$light <- table$light / light_max
  table <- data.frame(table)
  
  #FOR EACH CLASS, CLUSTERIZE!

  #Oversample class 1, and undersample class 2 (without presence)
  temp_table <- filter(table, action!=0)
  temp_table$action <- as.factor(temp_table$action)
  df1 <- SMOTE(form=action ~ ., data=temp_table, perc.over=600, perc.under=150)
  
  #Oversample class 0
  temp_table <- filter(table, action!=1)
  temp_table$action <- as.factor(temp_table$action)
  df2 <- SMOTE(form=action ~ ., data=temp_table, perc.over=600, perc.under=150)
  
  #Get a table with classes 0 and 1 oversampled, and class 2 undersampled
  table <- rbind(df1, filter(df2, action==0))
  table$light <- table$light * light_max
  table
}

#
# ACTION FUNCTIONS
#

# Detect which action occurred. Add a new column "action" where 0,1 = action taken
# and 2 = nothing occurred.
#   column is the column that will be analyzed for adges
snap.action.edge <- function(table, column) { 
    nrows <- length(table[column][[1]]);
    action <- "action";
    
    after <- table[column][2:nrows, 1];
    before <- table[column][1:(nrows - 1), 1];
    new <- after - before;
    
    for (i in 1:(nrows-1)) {
        if (abs(new[i, 1]) < 0.1) {
            new[i, 1] <- 2;
        } else if (new[i, 1] < 0){
            new[i, 1] <- 0;
        }
        else {
            new[i, 1] <- 1;
        }
    }
    
    table[action] <- rbind(new, 2);
    
    table
}