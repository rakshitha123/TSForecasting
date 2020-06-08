library(tsibble)
library(hashmap)

BASE_DIR <- "TSForecasting/ts_data/"
TIMESTAMP_NAME <- "start_timestamp"

#Extend these frequency lists as required
LOW_FREQUENCIES <- c("10_minutes", "half_hourly", "hourly")
LOW_FREQ_VALS <- c("10 min", "30 min", "1 hour")
HIGH_FREQUENCIES <- c("daily", "weekly", "monthly", "quarterly", "yearly")
HIGH_FREQ_VALS <- c("1 day", "1 week", "1 month", "3 months", "1 year")
FREQUENCIES <- c(LOW_FREQUENCIES, HIGH_FREQUENCIES)
FREQ_VALS <- c(LOW_FREQ_VALS, HIGH_FREQ_VALS)

#Create a hashmap containing possible frequency key-value pairs
FREQ_MAP <- hashmap(FREQUENCIES, FREQ_VALS)

forecast_horizon <- NULL
frequency <- NULL

#This function convert the contents in a .ts file into a tsibble or a dataframe
#file - .ts file path
#value_column_name - Any name that is preferred to have as the name of the column containing series values in the returning tsibble
#key - The name of the attribute that should be used as the key when creating the tsibble. If doesn't provide, a data frame will be returned
#index - The name of the time attribute that should be used as the key when creating the tsibble. If doesn't provide, a data frame will be returned
#time_attribute_name - The name of the time attribute. This can be same as the index parameter if the final goal is creating a tsibble
convert_ts_to_tsibble <-   function(file, value_column_name = "value", key = NULL, index = NULL, time_attribute_name = TIMESTAMP_NAME){
    if(is.character(file)) {
      file <- file(file, "r")
      on.exit(close(file))
    }
    if(!inherits(file, "connection"))
      stop("Argument 'file' must be a character string or connection.")
    if(!isOpen(file)) {
      open(file, "r")
      on.exit(close(file))
    }
    
    # Read meta-data
    col_names <- NULL

    line <- readLines(file, n = 1) #n is no: of lines to read
    
    while(length(line) && regexpr('^[[:space:]]*@data', line, perl = TRUE) == -1) { #Until read @data, run this loop (-1 indicate no match with the regular expression yet)
      
      if(regexpr('^[[:space:]]*@', line, perl = TRUE) > 0) { #If condition will be true for lines starting with @
        
        con <- textConnection(line)
        line <- scan(con, character(), quiet = TRUE) #Creating a vector containing the words in line (ex: "@attribute" "series_name")
        close(con)
        
        if(length(line) != 2)
          stop("Invalid meta-data specification.")
        else{
          if(line[1] == "@attribute")
            col_names <- c(col_names, line[2])
          else if(line[1] == "@frequency")
            frequency <- line[2]
          else if(line[1] == "@horizon")
            forecast_horizon <- as.numeric(line[2])
        }
      }
      line <- readLines(file, n = 1)
    }
    
    if(length(line) == 0)
      stop("Missing data section.")
    if(is.null(col_names))
      stop("Missing attribute section.")
    
    line <- readLines(file, n = 1)
    
    if(length(line) == 0)
      stop("Missing series information under data section.")
    
    for(col in col_names)
      assign(col, NULL)
    
    values <- NULL
    row_count <- 0
    
    # Get data
    while(length(line) != 0){
      full_info <- strsplit(line, ":")[[1]]
      
      if(length(full_info) != length(col_names)+1)
        stop("Missing attributes/values in series")
      
      series <- as.numeric(strsplit(tail(full_info, 1), ",")[[1]])
      
      if(is.na(series) || is.null(series) || series == "")
        stop("Missing series values")
      
      values <- c(values, series)
      row_count <- row_count + length(series)
      
      attributes <- head(full_info, length(full_info)-1)
      
      for(col in 1:length(col_names)){
        if(col_names[col] == time_attribute_name){
          if(is.null(frequency))
            stop("Frequency is missing")
          else{
            if(frequency %in% LOW_FREQUENCIES)
                start_time <- as.POSIXct(attributes[col], format = "%Y-%m-%d %H-%M-%S")
            else if(frequency %in% HIGH_FREQUENCIES)
                start_time <- as.Date(attributes[col], format = "%Y-%m-%d %H-%M-%S")
            else
                stop("Invalid frequency")
            
            if(is.na(start_time))
              stop("Incorrect timestamp format. Specify your timestamps as YYYY-mm-dd HH-MM-SS")
          }
          
          att <- seq(start_time, length=length(series), by=FREQ_MAP[[frequency]])
        }else{
          att <- eval(parse(text=col_names[col]))
          att <- c(att, rep(attributes[col], length(series)))
        }
        assign(col_names[col], att)
      }
      
      line <- readLines(file, n = 1)
    }
    
    data <- as.data.frame(matrix(nrow = row_count, ncol = length(col_names) + 1))
    colnames(data) <- c(col_names, value_column_name)
    
    for(col in col_names)
      data[[col]] <- eval(parse(text=col))
    
    data[[value_column_name]] <- values
    
    if(!(is.null(key) | is.null(index))){
      if(!(key %in% col_names & index %in% col_names))
        stop("Invalid key and index. Cannot convert the dataframe into tsibble")
      
      data <- as_tsibble(data, key = key, index = index)
    }
    
    data
}

options(pillar.sigfig = 7)
tsibble_data <- convert_ts_to_tsibble(paste0(BASE_DIR, "sample.ts"), "series_value", "series_name", "start_timestamp")




