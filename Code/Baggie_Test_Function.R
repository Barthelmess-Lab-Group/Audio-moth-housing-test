AM_baggie_csv <- function(path, csv = F, csv_name = "baggie_test.csv"){
  
  # This function takes the output (CSVs) from BirdNet from 'baggie test' audio moth test and combines them into one data frame
  # It also allows for a 'master' CSV to be written as part of the function as well, but for directly using the function in R there is no need to write the csv, so that's why it is a Boolean value
  # NOTE: This function is highly specific to the BirdNet output and the workflow we are using.
  # There is definitely a more general version of this function if need be
  
  # start with making sure that tidyverse is turned on
  require(tidyverse)
  
  # create an empty list that will hold the all the baggie files
  baggie_list <- list()
  
  # create an empty list that will hold the all the case files
  case_list <- list()
  
  # Lets get a list of all the .csvs first:
  csv_list <- list.files(path = path, full.names = T, recursive = T, pattern = "\\.csv$")
  
  # This loop checks if the a csv file is in either the baggie or case folder and adds it to each respective list
  for(i in csv_list){
    # checks if the file is an empty csv and if so skips to the next file in the list
    if(nrow(read.csv(i)) == 0)
      next
    else if(str_detect(i, "/baggie/")){ 
      b_df <- read.csv(i)
      # find the AM from the path and add it as a column with the ID filled in
      AM_b <- dirname(i) |> dirname() |> basename()
      b_df$AM_ID <- rep(AM_b, nrow(b_df))
      
      # Same here but using the folder name and converting it to a date
      date_b <- dirname(i) |> basename() |> ymd()
      b_df$date <- rep(date_b, nrow(b_df))
      
      # update the list containing all the baggie AM data frames
      # list() wraps the new data frame as a new element in the list
      baggie_list <- c(baggie_list, list(b_df)) 
      
    } else if (str_detect(i, "/case/")){
      c_df <- read.csv(i)
      # find the AM from the path and add it as a column with the ID filled in
      AM_c <- dirname(i) |> dirname() |> basename()
      c_df$AM_ID <- rep(AM_c, nrow(c_df))
      
      # Same here but using the folder name and converting it to a date
      date_c <- dirname(i) |> basename() |> ymd()
      c_df$date <- rep(date_c, nrow(c_df))
      
      # update the list containing all the baggie AM data frames
      case_list <- c(case_list, list(c_df)) 
      
    }
  }
  
  
  # Read in the baggie list
  baggie_df <- bind_rows(baggie_list)
  
  # Fill in "baggie" for all the values in the housing column
  baggie_df$housing <- rep("baggie", nrow(baggie_df))
  
  # Read in the case list
  case_df <- bind_rows(case_list)
  
  #Fill in "case for all the values in the housing column
  case_df$housing <- rep("case", nrow(case_df))
  
  # Merge the two dataframes
  dframe <- bind_rows(baggie_df, case_df)
  
  # Remove the 'X' column
  dframe$X <- NULL
  
  # Now check if we want to write the csv
  if(csv == T){
    write.csv(dframe, csv_name)
  }
  
  return(dframe)
}