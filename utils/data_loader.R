options(pillar.sigfig = 7)


# Extend these frequency lists as required
LOW_FREQUENCIES <- c("10_minutes", "half_hourly", "hourly")
LOW_FREQ_VALS <- c("10 min", "30 min", "1 hour")
HIGH_FREQUENCIES <- c("daily", "weekly", "monthly", "quarterly", "yearly")
HIGH_FREQ_VALS <- c("1 day", "1 week", "1 month", "3 months", "1 year")
FREQUENCIES <- c(LOW_FREQUENCIES, HIGH_FREQUENCIES)
FREQ_VALS <- c(LOW_FREQ_VALS, HIGH_FREQ_VALS)


# Create a hashmap containing possible frequency key-value pairs
FREQ_MAP <- list()

for(f in 1:length(FREQUENCIES))
  FREQ_MAP[[FREQUENCIES[f]]] <- FREQ_VALS[f]


# This function converts the contents in a .ts file into a tsibble or a dataframe and returns it along with other meta-data of the dataset: frequency, horizon, whether the dataset contains missing values and whether the series have equal lengths
#
# Parameters
# file - .ts file path
# value_column_name - Any name that is preferred to have as the name of the column containing series values in the returning tsibble
# key - The name of the attribute that should be used as the key when creating the tsibble. If doesn't provide, a data frame will be returned instead of a tsibble
# index - The name of the time attribute that should be used as the index when creating the tsibble. If doesn't provide, a data frame will be returned instead of a tsibble
convert_ts_to_tsibble <-   function(file, value_column_name = "series_value", key = NULL, index = NULL){
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
    col_types <- NULL
    frequency <- NULL
    forecast_horizon <- NULL
    contain_missing_values <- NULL
    contain_equal_length <- NULL

    line <- readLines(file, n = 1) #n is no: of lines to read

    while(length(line) && regexpr('^[[:space:]]*@data', line, perl = TRUE) == -1) { #Until read @data, run this loop (-1 indicate no match with the regular expression yet)

      if(regexpr('^[[:space:]]*@', line, perl = TRUE) > 0) { #This condition will be true for lines starting with @

        con <- textConnection(line)
        line <- scan(con, character(), quiet = TRUE) #Creating a vector containing the words in a line (ex: "@attribute" "series_name" "string")
        close(con)

        if(line[1] == "@attribute"){
          if(length(line) != 3)  #Attributes have both name and type
            stop("Invalid meta-data specification.")

          col_names <- c(col_names, line[2])
          col_types <- c(col_types, line[3])
        }else{
          if(length(line) != 2) #Other meta-data have only values
            stop("Invalid meta-data specification.")

          if(line[1] == "@frequency")
            frequency <- line[2]
          else if(line[1] == "@horizon")
            forecast_horizon <- as.numeric(line[2])
          else if(line[1] == "@missing")
            contain_missing_values <- as.logical(line[2])
          else if(line[1] == "@equallength")
            contain_equal_length <- as.logical(line[2])
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
        stop("Missing attributes/values in series.")

      series <- as.numeric(strsplit(tail(full_info, 1), ",")[[1]])

      if(sum(is.na(series)) == length(series))
        stop("All series values are missing. A given series should contains a set of comma separated numeric values. At least one numeric value should be there in a series.")

      values <- c(values, series)
      row_count <- row_count + length(series)

      attributes <- head(full_info, length(full_info)-1)

      for(col in 1:length(col_names)){

        att <- eval(parse(text=col_names[col]))

        #This format supports 3 attribute types: string, numeric and date
        if(col_types[col] == "date"){
          if(is.null(frequency))
            stop("Frequency is missing.")
          else{
            if(frequency %in% LOW_FREQUENCIES)
                start_time <- as.POSIXct(attributes[col], format = "%Y-%m-%d %H-%M-%S")
            else if(frequency %in% HIGH_FREQUENCIES)
                start_time <- as.Date(attributes[col], format = "%Y-%m-%d %H-%M-%S")
            else
                stop("Invalid frequency.")

            if(is.na(start_time))
              stop("Incorrect timestamp format. Specify your timestamps as YYYY-mm-dd HH-MM-SS")
          }

          att <- append(att, seq(start_time, length=length(series), by=FREQ_MAP[[frequency]]))
        }else{
          if(col_types[col] == "numeric")
            attributes[col] <- as.numeric(attributes[col])
          else if(col_types[col] == "string")
            attributes[col] <- as.character(attributes[col])
          else
            stop("Invalid attribute type.")

          if(is.na(attributes[col]))
            stop("Invalid attribute values.")

          att <- append(att, rep(attributes[col], length(series)))
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
        stop("Invalid key and index. Cannot convert the dataframe into tsibble format.")

      data <- tsibble:::build_tsibble(x = data, key = key, index = index, ordered = F)
    }

    list(data, frequency, forecast_horizon, contain_missing_values, contain_equal_length)
}


# Example of usage
# loaded_data <- convert_ts_to_tsibble("TSForecasting/ts_data/sample.ts", "series_value", "series_name", "start_timestamp")
# tsibble_data <- loaded_data[[1]]
# frequency <- loaded_data[[2]]
# forecast_horizon <- loaded_data[[3]]
# contain_missing_values <- loaded_data[[4]]
# contain_equal_length <- loaded_data[[5]]
