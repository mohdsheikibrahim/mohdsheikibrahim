# Defining Data Summarizer Function in R
# Author : Mohamed Sheik Ibrahim
# Email : mohamedsheik.ibrahim@libertyanalytics.com
###################################################################

#Function Definition

data_summarizer <- function(in_data,output_in_xlsx=F)
{
  
  
  packages <- c("zoo", "pastecs", "reshape","dplyr")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  } 
  
  library(zoo)
  library(pastecs)
  library(reshape)
  library(dplyr)
  
  # Function to fetch top 10 values ----
  top_values<-function(in_data,restrict_levels_to=10) 
  {
    
   data_char<-in_data
   
    # creating a dummy dataframe
    var_name <- character(0)
    levels <- character(0)
    V1 <- numeric(0)
    final_freq<-data.frame(var_name,levels,V1)
    
    # loop to apply table function to all variables
    for (i in 1:length(data_char)) {
      
      df1<-as.data.frame(as.matrix(table(data_char[i],useNA = "ifany")))
      
      row_names_df1<-row.names(df1)
      
      row_names_df1[is.na(row_names_df1)]<-"NA"
      
      
      df2<-as.data.frame(row_names_df1)
      row.names(df1)<-NULL
      df3<-cbind(df1,df2)
      
      
      
      df3$var_name <- colnames(data_char[i])
      
      final_freq <- rbind(final_freq,df3)
      
    }
    
    # renaming the variables ----
    final_freq<- reshape::rename(final_freq,c("V1"="frequency","row_names_df1"="levels"))
    
    # changing the order of the variables 
    final_freq<-final_freq[c("var_name","levels","frequency")]
    
    # creating summary ----
    summ_filter_var<-final_freq %>%
      group_by(var_name)%>%
      select(var_name,levels,frequency)%>%
      summarise(no_of_levels=n(),tot_count=sum(frequency)
      )
    
    # function to formatting the percentage column -----
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    
    
    final_freq_2 <- merge(final_freq,summ_filter_var,all.x=TRUE)
    # removing the key variables -----
    final_freq_2 <- final_freq_2%>% group_by(var_name)%>%
      
      mutate(percentage=percent(frequency/tot_count))%>%
      arrange(var_name,desc(percentage)) %>%
      slice(1:restrict_levels_to)
    
    
    
    # removing the total count
    final_freq_2 <- final_freq_2 %>% select(-tot_count)
    
    
    return(final_freq_2)
  }
  
 
    #separating columns based on their type into different tables
  data_num <- in_data [sapply(in_data , is.numeric)]
  
  if (length(data_num) == 0) {
    data_num <- data.frame(No_Numeric_var <- 1)
  }
  
  data_date <-
    in_data[sapply(in_data, function(var_name) {
      #class of the variable 
      class_var <-class(var_name)
      ifelse(class_var[1] == "Date", T, F)
    })]
  
  
  # assigning dummy value if no such data type is available ----
  if (length(data_date) == 0) {
    data_date <- data.frame(No_Date_var <- 0)
  }
  
  
  data_char <- in_data [sapply(in_data , is.character)]
  
  data_logic <- in_data [sapply(in_data, is.logical)]
  
  data_factor <- in_data[sapply(in_data, is.factor)]
  
  if (length(data_logic) > 0) {
    data_char <- cbind(data_char, data_logic)
    
  }
  if (length(data_factor) > 0) {
    data_char <- cbind(data_char, data_factor)
  }
  
  if (length(data_char) == 0)  {
    data_char <- data.frame(No_Char_var <- "No_Char_var")
  }
  
  # Assigning the variable types other than vars, char, date, factor into a new dataframe and
  # add it with  the data_char
  names_defined <-
    c(colnames(data_char),
      colnames(data_num),
      colnames(data_date))
  
  names_input <- colnames(in_data)
  
  names_not_defined <-
    names_input[which(!names_input %in% names_defined)]
  
  data_others <- in_data[c(names_not_defined)]
  
  data_char <- cbind(data_char, data_others)
  # Stats Description
  # Numeric  & date variables
  
  pol_stat_num <- stat.desc(data_num)
  pol_stat_date <- stat.desc(data_date)
  # transposing stats table
  
 
  t2 <- t(as.matrix(pol_stat_num))
  
  trans_stat_num <- as.data.frame(t2)
  trans_stat_num$type <- "Numeric"
  
  t3 <- t(as.matrix(pol_stat_date))
  
  trans_stat_date <- as.data.frame(t3)
  trans_stat_date$type <- "Date"
  
  # Re formatting date characteristics ----
  trans_stat_date$median <- as.Date(trans_stat_date$median)
  
  trans_stat_date$min <- as.Date(trans_stat_date$min)
  trans_stat_date$max <- as.Date(trans_stat_date$max)
  
  
  trans_stat_date$sum <- NA
  trans_stat_date$std.dev <- NA
  trans_stat_date$SE.mean <- NA
  trans_stat_date$CI.mean.0.95 <- NA
  trans_stat_date$var <- NA
  trans_stat_date$coef.var <- NA
  trans_stat_date$mean <- NA
  
  #calculating total number of observations ----
  trans_stat_date$tot_obs <- trans_stat_date$nbr.val + trans_stat_date$nbr.na
  trans_stat_num$tot_obs <- trans_stat_num$nbr.val + trans_stat_num$nbr.na
  #calculated as percentage of non missing values
  trans_stat_num$pct_of_zeros_or_blanks <-
    trans_stat_num$nbr.null / trans_stat_num$tot_obs
  trans_stat_date$pct_of_zeros_or_blanks <-
    trans_stat_date$nbr.null / trans_stat_date$tot_obs
  #calculated as percentage of total observation
  trans_stat_num$pct_of_missing <-
    trans_stat_num$nbr.na / trans_stat_num$tot_obs
  trans_stat_date$pct_of_missing <-
    trans_stat_date$nbr.na / trans_stat_date$tot_obs
  #Percentile calculation
  
  quant_fn <- function(varname)
  {
    quant_vectr <-
      quantile(
        varname,
        probs = c(0.01, 0.05, 0.1, 0.25, 0.75, 0.9, 0.95, 0.99),
        na.rm = TRUE
      )
    
  }
  
  percentile <- sapply(data_num, quant_fn)
  
  
  percentile_df <- as.data.frame(t(percentile))
  
  #counting the unique values
  unique_fn <- function (varname)
  {
    cnt_unique_raw <- length(unique(varname))
    cnt_unique_clean <-
      length(unique(toupper(trimws(varname, which = "both"))))
    cnt_unique <- cbind(cnt_unique_raw, cnt_unique_clean)
    return((cnt_unique))
    
  }
  
  unique_count <- as.data.frame(t(sapply(data_num, unique_fn)))
  
  unique_count_date <- as.data.frame(t(sapply(data_date, unique_fn)))
  
  # Top values with its percentage ----
  # Numeric variables
  top_value_table_num<-top_values(data_num)
  top_value_table_num$top_10_values <- paste(top_value_table_num$levels,top_value_table_num$percentage,sep = " : ")
  top_value_vector_num <- aggregate(top_10_values~var_name,FUN = paste,collapse=",  ",data = top_value_table_num)
  rownames(top_value_vector_num)<-top_value_vector_num$var_name
  
  # Date variables
  top_value_table_date<-top_values(data_date)
  top_value_table_date$top_10_values <- paste(top_value_table_date$levels,top_value_table_date$percentage,sep = " : ")
  top_value_vector_date <- aggregate(top_10_values~var_name,FUN = paste,collapse=",  ",data = top_value_table_date)
  
  
  data_num_EDD <- cbind(trans_stat_num, percentile_df, unique_count)
  data_num_EDD<-data_num_EDD[order(row.names(data_num_EDD)),]
  data_num_EDD <- cbind(data_num_EDD,top_value_vector_num["top_10_values"])
  
  
  data_date_EDD <- cbind(trans_stat_date, unique_count_date)
  data_date_EDD<-data_date_EDD[order(row.names(data_date_EDD)),]
  data_date_EDD <- cbind(data_date_EDD,top_value_vector_date["top_10_values"])
  
  #assigning NA to variables not in data_num_EDD ----
  data_date_EDD$`1%` <- NA
  data_date_EDD$`5%` <- NA
  data_date_EDD$`10%` <- NA
  data_date_EDD$`25%` <- NA
  data_date_EDD$`75%` <- NA
  data_date_EDD$`90%` <- NA
  data_date_EDD$`95%` <- NA
  data_date_EDD$`95%` <- NA
  data_date_EDD$`99%` <- NA
  
  
  # Stats Descriptional
  # Character variables----
  
  
  #counting non_missing values----
  obs_count <- function(var_name)
  {
    nonmissing_obs <- sum(!is.na(var_name))
    
  }
  char_obs <- sapply(data_char, obs_count)
  char_obs_v2 <- as.data.frame(as.matrix(char_obs))
  char_obs_v2 <- reshape::rename(char_obs_v2, c("V1" = "nbr.val"))
  
  #counting missing values----
  miss_count <- function(var_name)
  {
    missing_obs <- sum(is.na(var_name))
  }
  miss_obs <- sapply(data_char, miss_count)
  miss_obs_v2 <- as.data.frame(as.matrix(miss_obs))
  miss_obs_v2 <- reshape::rename(miss_obs_v2, c("V1" = "nbr.na"))
  #counting blank values----
  blank_count <- function(var_name)
  {
    blank_obs <- sum(var_name %in% c("", " "))
  }
  
  blank_obs <- sapply(data_char, blank_count)
  blank_obs_v2 <- as.data.frame(as.matrix(blank_obs))
  blank_obs_v2 <- reshape::rename(blank_obs_v2, c("V1" = "nbr.null"))
  
  # creating unique values count----
  unique_count_char <- as.data.frame(t(sapply(data_char, unique_fn)))
  
  # adding the variable class/type ----
  type_list <- as.data.frame(as.matrix(sapply(data_char, class)))
  type_list <- reshape::rename(type_list, c("V1" = "type"))
  
  # top 10 values for character variables ----
  top_value_table_char<-top_values(data_char)
  top_value_table_char$top_10_values <- paste(top_value_table_char$levels,top_value_table_char$percentage,sep = " : ")
  top_value_vector_char <- aggregate(top_10_values~var_name,FUN = paste,collapse=",  ",data = top_value_table_char)
  
  
  #merging the columns created so far using variable name column as key
  data_char_EDD <-
    cbind(char_obs_v2,
          blank_obs_v2,
          miss_obs_v2,
          unique_count_char,
          type_list)
  
  data_char_EDD<-data_char_EDD[order(row.names(data_char_EDD)),]
  data_char_EDD <- cbind(data_char_EDD,top_value_vector_char["top_10_values"])
  
  #calculating total observation count ----
  data_char_EDD$tot_obs<-data_char_EDD$nbr.na+data_char_EDD$nbr.val
  
  #calculated as a percentage of non NA/Missing observations
  data_char_EDD$pct_of_zeros_or_blanks <-
    data_char_EDD$nbr.null / data_char_EDD$tot_obs
  
  #calculated as percentage of total observations
  data_char_EDD$pct_of_missing <-
    data_char_EDD$nbr.na / data_char_EDD$tot_obs
  
  
  
  
  #Appending the numeric and character dataframes
  # for rbind both the table's column names should match.
  
  data_num_EDD.names <- names(data_num_EDD)
  data_char_EDD.names <- names(data_char_EDD)
  
  
  #columns in data_num_EDD but not in data_char_EDD
  data_char_EDD.add <- setdiff(data_num_EDD.names, data_char_EDD.names)
  
  #adding blank columns to data_char_EDD
  
  for (i in 1:length(data_char_EDD.add)) {
    data_char_EDD[data_char_EDD.add[i]] <- NA
  }
  
  
  
  # changing the datatypes of all columns to character in order to preserve the format difference----
  data_char_EDD[] <- lapply(data_char_EDD, as.character)
  data_num_EDD[] <- lapply(data_num_EDD, as.character)
  data_date_EDD[] <- lapply(data_date_EDD, as.character)
  # combining all small dataframes ----
  data_EDD <- rbind(data_num_EDD, data_char_EDD, data_date_EDD)
  
  data_EDD$variable_name <-row.names(data_EDD)
  row.names(data_EDD) <- NULL
  
  
  #Re-ordering the variables
  data_EDD_v2 <-
    data_EDD[c(
      "variable_name",
      "type",
      "tot_obs",
      "nbr.val",
      "nbr.null",
      "pct_of_zeros_or_blanks",
      "nbr.na",
      "pct_of_missing",
      "V1",
      "V2",
      "mean",
      "sum",
      "std.dev",
      "min",
      "1%",
      "5%",
      "10%",
      "25%",
      "median",
      "75%",
      "90%",
      "95%",
      "99%",
      "max",
      "top_10_values"
    )]
  #
  # #Renaming variables
  out_data <- reshape::rename(
    data_EDD_v2,
    c("tot_obs"="total_observations",
      "nbr.val" = "nonmissing_Obs",
      "nbr.null" = "zero_or_blank_count",
      "nbr.na" = "missing_count",
      "V1" = "Unique_count_RAW",
      "V2" = "Unique_count_cleaned",
      "1%" = "p1",
      "5%" = "p5",
      "10%" = "p10",
      "25%" = "p25",
      "75%" = "p75",
      "90%" = "p90",
      "95%" = "p95",
      "99%" = "p99"
      
    )
  )
  
  out_data<-out_data[(! out_data$variable_name %in% c("No_Char_var.....No_Char_var.","No_Date_var....0","No_Numeric_var....1")),]
  
  rownames(out_data) <- seq(length=nrow(out_data))
  
 # Creating csv file 
  
#  if(!missing(output_in_xlsx)){
  if(output_in_xlsx==T) { 
    
    
      write.csv(out_data,file="output_csv_EDA.csv")
    
    
    shell(shQuote(normalizePath("DS_code_txt_Updated.vbs")), "cscript", flag = "//nologo") 
  }
#  }
  
  return(out_data)
}





