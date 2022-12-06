# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(arrow)

#----- Country Key Details-----#
# Child code that returns tibble
country_info <- function(col_name, col_value){
  # read the data
  all_data <- arrow::read_parquet(file="data/FinalCompleteData.parquet")

  # check if parameter exists
  if(sum(stringr::str_detect(all_data
                             %>%select({{col_name}})
                             %>%collect()
                             %>% as.matrix
                             %>%as.vector(), paste("^", col_value, "$", sep=""))) <= 0) {
    stop(sprintf("%s is not contained in our dataset.", {{col_value}}))
  }

  all_data %>%
    dplyr::filter({{col_name}}==col_value) %>%
    select(c(Country_Name, Long_Name, Country_Code, Currency_Unit, Region, Income_Group, Head_of_state, Head_of_government, Gender_HoS)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_longer(cols = c(Country_Name, Long_Name, Country_Code, Currency_Unit, Region, Income_Group, Head_of_state, Head_of_government, Gender_HoS),
                        names_to = "Columns",
                        values_to = "Values")
}



# -----Parent code for checking constraints-----#
country_key_details <- function(country_name, country_code) {
  # stop if no arguments were provided
  if(missing(country_name) & missing(country_code)){
    stop("No arguments were provided.")
  }

  # for conditions where only country_name is provided
  if (!missing(country_name) & missing(country_code)){
    # Check if parameter is valid
    if(!is.character(country_name)){
      stop(sprintf("country_name and country_code must be strings"))
    }
    return(country_info(col_name=Country_Name, col_value = stringr::str_to_title(country_name)))
  }

  # for conditions where only country_code is provided
  else if (missing(country_name) & !missing(country_code)){
    # Check if parameter is valid
    if(!is.character(country_code)){
      stop(sprintf("country_name and country_code must be strings"))
    }
    return(country_info(col_name = Country_Code, col_value = stringr::str_to_upper(country_code)))
  }

  # for conditions were both country_name and country_code are provided
  else if (!missing(country_name) & !missing(country_code)){
    # Check if parameter is valid
    if(!is.character(country_name) | !is.character(country_code)){
      stop(sprintf("country_name and country_code must be strings"))
    }
    # first try with the country_name
    attempt <- try(country_info(col_name = Country_Name, col_value = stringr::str_to_title(country_name)), silent=TRUE)
    if(inherits(attempt, "try-error")){
      # try with country code if country name fails
      options(warn = 1)
      warning("Country Name Not found. Trying Country code.......")
      attempt2 <- try(country_info(col_name = Country_Code, col_value = stringr::str_to_upper(country_code)), silent=TRUE)
      if(inherits(attempt2, "try-error")){
        return(c(attempt, attempt2))
      } else {
        return(attempt2)
      }
    } else {
      return(attempt)
    }
  }
}



# ---------- Function for extracting country leaders ------------
country_leader <- function(country_name, country_code) {
  # call the country_key_details function
  final_results <- country_key_details(country_name, country_code)

  if(TRUE %in% (class(final_results) == c("tbl_df", "tbl", "data.frame"))) {
    return(final_results %>% dplyr::filter(Columns=="Head_of_state" | Columns=="Head_of_government" | Columns=="Gender_HoS"))
  } else {
    stop(final_results)
  }
}


# ---------- Function for extracting vector of all existing countries in our dataset------------
all_countries <- function(){
  # read the data
  all_data <- arrow::read_parquet(file="data/FinalCompleteData.parquet")
  result <- all_data %>% dplyr::select(Country_Name) %>% dplyr::distinct()%>%collect()%>% as.matrix%>%as.vector()
  return(result)
}


# ---------- Function for extracting vector of all World Bank indicators in our dataset------------
all_indicators <- function(){
  # read the data
  all_data <- arrow::read_parquet(file="data/FinalCompleteData.parquet")
  result <- all_data %>% dplyr::select(Indicator_Name) %>% dplyr::distinct()%>%collect()%>% as.matrix%>%as.vector()
  return(result)
}


# ---------- Function for extracting tibble of entire dataset used in package------------
all_data <- function(){
  # read the data
  all_data <- arrow::read_parquet(file="data/FinalCompleteData.parquet")
  return(all_data)
}



# ----Function to return Certain Indicators per country in a year
# Child function
indicators_info <- function(col_name, col_value, indicator_contains, year){
  # read the data
  all_data <- arrow::read_parquet(file="data/FinalCompleteData.parquet")


  #check if parameter "year" exists
  if(!(year %in% colnames(all_data))){
    stop(sprintf("Values with year %s does not exist in the dataset.", year))
  }


  # check if parameter "indicator_contains" exists
  if(sum(stringr::str_detect(all_data
                             %>%select(Indicator_Name)
                             %>%collect()
                             %>% as.matrix
                             %>%as.vector(), paste("(?i)", indicator_contains, sep=""))) <= 0) {
    stop(sprintf("An indicator containing %s is not contained in the dataset.", indicator_contains))
  }


  # check if parameter "col_value" exists
  if(sum(stringr::str_detect(all_data
                             %>%select({{col_name}})
                             %>%collect()
                             %>% as.matrix
                             %>%as.vector(), paste("^", col_value, "$", sep=""))) <= 0) {
    stop(sprintf("%s is not contained in our dataset.", {{col_value}}))
  }


  return(all_data %>%
           dplyr::filter({{col_name}}==col_value & str_detect(Indicator_Name, paste("(?i)", indicator_contains, sep=""))) %>% select(c(3, 5:66)) %>%
           tidyr::pivot_longer(cols = c(2:63), names_to = "Year", values_to = "Values") %>%
           filter(Year==year))
}

# Parent function
country_indicator <- function(country_name, country_code, indicator_contains, year){
  # check if arguments indicator_contains, year are missing
  if(missing(indicator_contains) & missing(year)){
    stop("Arguments: indicator_contains and year must be provided.")
  }
  if(missing(country_name) & missing(country_code)){
    stop("One of country_name or country_code must be provided")
  }


  # for conditions where only country_name is provided
  if (!missing(country_name) & missing(country_code)){

    # Check if all parameters are valid
    if(!is.character(country_name) | !is.character(indicator_contains) | !is.character(year)){
      stop("All arguments must provided as strings")
    }
    return(indicators_info(col_name=Country_Name, col_value = stringr::str_to_title(country_name), indicator_contains = indicator_contains, year = year))
  }


  # for conditions where only country_code is provided
  else if (missing(country_name) & !missing(country_code)){
    # Check if all parameters are valid
    if(!is.character(country_code) | !is.character(indicator_contains) | !is.character(year)){
      stop(sprintf("All arguments must provided as strings"))
    }
    return(indicators_info(col_name = Country_Code, col_value = stringr::str_to_upper(country_code),
                           indicator_contains = indicator_contains, year = year))
  }


  # for conditions were both country_name and country_code are provided
  else if (!missing(country_name) & !missing(country_code)){

    # Check if all parameters are valid
    if(!is.character(country_name) | !is.character(country_code) | !is.character(indicator_contains) | !is.character(year)){
      stop(sprintf("All arguments must provided as strings"))
    }

    # first try with the country_name
    attempt <- try(indicators_info(col_name=Country_Name, col_value = stringr::str_to_title(country_name), indicator_contains = indicator_contains, year = year), silent=TRUE)
    if(inherits(attempt, "try-error")){
      # try with country code if country name fails
      options(warn = 1)
      warning("Country Name Not found. Trying Country code.......")
      attempt2 <- try(indicators_info(col_name = Country_Code, col_value = stringr::str_to_upper(country_code),
                                      indicator_contains = indicator_contains, year = year), silent=TRUE)
      if(inherits(attempt2, "try-error")){
        return(c(attempt, attempt2))
      } else {
        return(attempt2)
      }
    } else {
      return(attempt)
    }
  }
}


# -----------Rank Countries by Indicator-------------#
rank_indicators_by_country <- function(indicator_name, year="2021", n=231, pos=1) {
  # check if indicator_name is missing
  if(missing(indicator_name)){
    stop("indicator_name must be provided.")
  }

  # check if parameters are valid
  if(!is.character(indicator_name)){stop("indicator_name must be a string.")}
  if(!is.character(year)){stop("year must be provided as a string.")}

  if(!is.numeric(n)){
    stop("n must be an integer.")
  }

  if((n < 1) | (n > 231)){
    stop("n must have values between 1 and 231.")
  }

  # Just in case a decimal is provided
  n = ceiling(n)

  if(pos != 1 & pos != -1){
    stop("pos must either 1 or -1. Use 1 for Top result and -1 for bottom results.")
  }

  # read the data
  all_data <- arrow::read_parquet(file="data/FinalCompleteData.parquet")

  #check if parameter "year" exists
  if(!(year %in% colnames(all_data))){
    stop(sprintf("Values with year %s does not exist in the dataset.", year))
  }

  # check if indicator_name exists
  indicators_vec <- all_data %>%dplyr::select(Indicator_Name) %>% collect() %>% as.matrix %>% as.vector()
  if(!indicator_name %in% indicators_vec) {
    #if indicator_name does not exist
    indicators_like <- all_data %>% dplyr::select(Indicator_Name) %>% dplyr::distinct() %>%dplyr::filter(str_detect(Indicator_Name, paste("(?i)", indicator_name, sep=""))) %>% collect() %>% as.matrix %>% as.vector()

    if(length(indicators_like > 0)){
      options(warn = 1)
      warning("indicator_name not found. Did you mean:")

      warning(cat(indicators_like, sep = " || "))
    }
    stop(sprintf("The indicator - %s is not contained in our dataset. Run all_indicators_like(indicator_name) to see names of indicators.", indicator_name))
  } else{
    # If indicator_name exists
    final<- all_data %>%
      dplyr::filter(Indicator_Name==indicator_name) %>%
      dplyr::select(c(1, 5:66)) %>%
      mutate(dplyr::across(dplyr::starts_with("1")|dplyr::starts_with("2"), ~as.numeric(gsub(",", ".", .x)))) %>%
      tidyr::pivot_longer(cols = c(2:63), names_to="Year", values_to = "values") %>%
      dplyr::filter(Year==year) %>%
      dplyr::top_n(n*pos)
    if (pos==-1){return(final%>%dplyr::arrange(values))}else{return(final%>%dplyr::arrange(desc(values)))}
  }
}



# -------- Return all indicators like a given strig
all_indicators_like <- function(like){
  if (missing(like)){
    stop("like parameter must be provided.")
  }
  # read the data
  all_data <- arrow::read_parquet(file="data/FinalCompleteData.parquet")
  indicators_like <- all_data %>% dplyr::select(Indicator_Name) %>% dplyr::distinct() %>%dplyr::filter(str_detect(Indicator_Name, paste("(?i)", like, sep=""))) %>% collect() %>% as.matrix %>% as.vector()
  if(length(indicators_like)>0){return(indicators_like)} else{
    options(warn = 1)
    warning(sprintf("No indicator like %s found", like))
    return()
  }
}


