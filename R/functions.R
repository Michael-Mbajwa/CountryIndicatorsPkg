# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(arrow)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)


#' Get Key Country Details
#' @description Helper function for country_key_details.
#' @param col_name The column name to be extracted from the loaded data frame.
#' @param col_value The value which is used to filter col_name
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @importFrom dplyr filter select distinct collect
#' @importFrom tidyr pivot_longer
#' @return A data frame
#' @noRd
#' @keywords internal
#' @details
#' This is a reusable function that allows other functions to create a filtered data frame with
#' a specific column of interest having specified values.
.country_info <- function(col_name, col_value){
  # read the data
  all_data <- FinalCompleteData

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
    dplyr::select(c(Country_Name, Long_Name, Country_Code, Currency_Unit, Region, Income_Group, Head_of_state, Head_of_government, Gender_HoS)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_longer(cols = c(Country_Name, Long_Name, Country_Code, Currency_Unit, Region, Income_Group, Head_of_state, Head_of_government, Gender_HoS),
                        names_to = "Columns",
                        values_to = "Values")
}



#' Return Key Country Info
#' @description This function returns important information regarding specified country.
#' @param country_name The name of the country whose details the user wants to extract.
#' @param country_code The country code of the country.
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_title str_to_upper
#' @export
#' @return A data frame
#' @details
#' This function allows the user to create a data frame containing general overview of a country.
#' The information returned contains Country Name, Country Code, Currency, Unit, Income Group,
#' Head of State, and gender of head of state.
#' Either one of the parameters must be provided.
#' @examples
#' # See country details for America using country code
#' library("CountryIndicatorsPkg")
#' country_key_details(country_code = "usa")
#' # See country details for Nigeria using country Name
#' country_key_details(country_name = "nigeria")
#' @author Michael Mbajwa
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
    return(.country_info(col_name=Country_Name, col_value = stringr::str_to_title(country_name)))
  }

  # for conditions where only country_code is provided
  else if (missing(country_name) & !missing(country_code)){
    # Check if parameter is valid
    if(!is.character(country_code)){
      stop(sprintf("country_name and country_code must be strings"))
    }
    return(.country_info(col_name = Country_Code, col_value = stringr::str_to_upper(country_code)))
  }

  # for conditions were both country_name and country_code are provided
  else if (!missing(country_name) & !missing(country_code)){
    # Check if parameter is valid
    if(!is.character(country_name) | !is.character(country_code)){
      stop(sprintf("country_name and country_code must be strings"))
    }
    # first try with the country_name
    attempt <- try(.country_info(col_name = Country_Name, col_value = stringr::str_to_title(country_name)), silent=TRUE)
    if(inherits(attempt, "try-error")){
      # try with country code if country name fails
      options(warn = 1)
      warning("Country Name Not found. Trying Country code.......")
      attempt2 <- try(.country_info(col_name = Country_Code, col_value = stringr::str_to_upper(country_code)), silent=TRUE)
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



#' View Country Leader
#' @description This function returns the country leader for the specified country.
#' @param country_name The name of the country whose country_leader the user wants to extract.
#' @param country_code The country code of the country.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter collect
#' @export
#' @return A data frame
#' @details
#' This function allows the user to create a data frame containing general information of a country's leader.
#' The information returned contains, Head of State name, and gender of head of state.
#' Either one of the parameters must be provided.
#' See the examples below for its usage.
#' @examples
#' # See country leader details for America using country code
#' country_leader(country_code = "usa")
#' # See country leader details for Luxembourg using country name
#' country_leader(country_name = "luxembourg")
#' @author Michael Mbajwa
country_leader <- function(country_name, country_code) {
  # call the country_key_details function
  final_results <- country_key_details(country_name, country_code)

  if(TRUE %in% (class(final_results) == c("tbl_df", "tbl", "data.frame"))) {
    return(final_results %>% dplyr::filter(Columns=="Head_of_state" | Columns=="Head_of_government" | Columns=="Gender_HoS"))
  } else {
    stop(final_results)
  }
}



#' View All Countries
#' @description This function returns names of all the countries in the world.
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct collect
#' @export
#' @return A vector
#' @details
#' This function allows the user to create a vector of all countries.
#' @author Michael Mbajwa
all_countries <- function(){
  # read the data
  all_data <- FinalCompleteData
  result <- all_data %>% dplyr::select(Country_Name) %>% dplyr::distinct() %>% collect() %>% as.matrix %>% as.vector()
  return(result)
}



#' All World Bank Indicators
#' @description This function extracts vector of World Bank indicators.
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct collect
#' @export
#' @return A vector
#' @details
#' This function allows the user to create a vector of World Bank indicators.
#' @author Michael Mbajwa
all_indicators <- function(){
  # read the data
  all_data <- FinalCompleteData
  result <- all_data %>% dplyr::select(Indicator_Name) %>% dplyr::distinct() %>% collect() %>% as.matrix %>% as.vector()
  return(result)
}




#' Helper Function
#' @description Helper function for country_indicator
#' @param col_name The column name to be extracted from the loaded data frame.
#' @param col_value The value which is used to filter col_name.
#' @param indicator_contains A string that indicator should contain.
#' @param year The year you are interested in. Must be a string.
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @importFrom dplyr filter select distinct collect
#' @importFrom tidyr pivot_longer
#' @return A data frame
#' @noRd
#' @keywords internal
#' @details
#' This is a reusable function to be used by other functions.
.indicators_info <- function(col_name, col_value, indicator_contains, year){
  # read the data
  all_data <- FinalCompleteData


  #check if parameter "year" exists
  if(!(year %in% colnames(all_data))){
    stop(sprintf("Values with year %s does not exist in the dataset.", year))
  }


  # check if parameter "indicator_contains" exists
  if(sum(stringr::str_detect(all_data
                             %>%dplyr::select(Indicator_Name)
                             %>%collect()
                             %>% as.matrix
                             %>%as.vector(), paste("(?i)", indicator_contains, sep=""))) <= 0) {
    stop(sprintf("An indicator containing %s is not contained in the dataset.", indicator_contains))
  }


  # check if parameter "col_value" exists
  if(sum(stringr::str_detect(all_data
                             %>%dplyr::select({{col_name}})
                             %>%collect()
                             %>% as.matrix
                             %>%as.vector(), paste("^", col_value, "$", sep=""))) <= 0) {
    stop(sprintf("%s is not contained in our dataset.", {{col_value}}))
  }


  return(all_data %>%
           dplyr::filter({{col_name}}==col_value & str_detect(Indicator_Name, paste("(?i)", indicator_contains, sep=""))) %>% select(c(3, 5:66)) %>%
           tidyr::pivot_longer(cols = c(2:63), names_to = "Year", values_to = "Values") %>%
           dplyr::filter(Year==year))
}





#' Specific Indicator for Specific Country
#' @description This function returns World Bank Indicators for a specified country.
#' @param country_name The name of the country whose details the user wants to extract.
#' @param country_code The country code of the country.
#' @param indicator_contains A string that world bank indicator should contain.
#' @param year The year you are interested in. Must be a string.
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_title str_to_upper
#' @export
#' @return A data frame
#' @details
#' This function allows the user to create a data frame containing key world bank indicators containing a
#' certain string. Indicators are filtered down to the year and country name.
#' Either one country code or country name must be provided.
#' See the example below for its usage.
#' @examples
#' # See country indicator containing health for America using country name
#' country_indicator(country_code = "usa", indicator_contains = "health", year = "2002")
#' @author Michael Mbajwa
country_indicator <- function(country_name, country_code, indicator_contains, year){
  # check if arguments indicator_contains, year are missing
  if(missing(indicator_contains) | missing(year)){
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
    return(.indicators_info(col_name=Country_Name, col_value = stringr::str_to_title(country_name), indicator_contains = indicator_contains, year = year))
  }


  # for conditions where only country_code is provided
  else if (missing(country_name) & !missing(country_code)){
    # Check if all parameters are valid
    if(!is.character(country_code) | !is.character(indicator_contains) | !is.character(year)){
      stop(sprintf("All arguments must provided as strings"))
    }
    return(.indicators_info(col_name = Country_Code, col_value = stringr::str_to_upper(country_code),
                           indicator_contains = indicator_contains, year = year))
  }


  # for conditions were both country_name and country_code are provided
  else if (!missing(country_name) & !missing(country_code)){

    # Check if all parameters are valid
    if(!is.character(country_name) | !is.character(country_code) | !is.character(indicator_contains) | !is.character(year)){
      stop(sprintf("All arguments must provided as strings"))
    }

    # first try with the country_name
    attempt <- try(.indicators_info(col_name=Country_Name, col_value = stringr::str_to_title(country_name), indicator_contains = indicator_contains, year = year), silent=TRUE)
    if(inherits(attempt, "try-error")){
      # try with country code if country name fails
      options(warn = 1)
      warning("Country Name Not found. Trying Country code.......")
      attempt2 <- try(.indicators_info(col_name = Country_Code, col_value = stringr::str_to_upper(country_code),
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



#' Rank Country By Indicator
#' @description This function ranks countries by their performance in a particular world bank indicator.
#' @param indicator_name The full indicator name the user is interested in.
#' @param year The year you are interested in. Must be a string.
#' @param n Number of countries to be ranked. Maximum is 231 and Minimum is 1.
#' @param pos View the countries in ascending or descending order. Use 1 for Top result and -1 for bottom results.
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_title str_to_upper
#' @importFrom dplyr filter select distinct top_n arrange across starts_with collect mutate
#' @importFrom tidyr pivot_longer
#' @export
#' @return A data frame
#' @details
#' This function allows the user to create a data frame containing the rank of countries according
#' to an indicator in a specified year.See the examples below for its usage.
#' @section Warning: Getting indicator name can be difficult. Run all_indicators_like(like)
#'         with like being a string you want your indicator to contain. From there you can select your indicator name.
#' @examples
#' # See the top 10 countries for a specific indicator in a specific year
#' rank_indicators_by_country("Current health expenditure per capita (current US$)", year="2002", n=10)
#' # See the bottom 10 countries for a specific indicator in a specific year
#' rank_indicators_by_country("Current health expenditure per capita (current US$)", year="2002", n=10, p=-1)
#' @author Michael Mbajwa
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
  all_data <- FinalCompleteData

  #check if parameter "year" exists
  if(!(year %in% colnames(all_data))){
    stop(sprintf("Values with year %s does not exist in the dataset.", year))
  }

  # check if indicator_name exists
  indicators_vec <- all_data %>% dplyr::select(Indicator_Name) %>% collect() %>% as.matrix %>% as.vector()
  if(!indicator_name %in% indicators_vec) {
    #if indicator_name does not exist
    indicators_like <- all_data %>% dplyr::select(Indicator_Name) %>% dplyr::distinct() %>% dplyr::filter(str_detect(Indicator_Name, paste("(?i)", indicator_name, sep=""))) %>% collect() %>% as.matrix %>% as.vector()

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
      dplyr::mutate(dplyr::across(dplyr::starts_with("1")|dplyr::starts_with("2"), ~as.numeric(gsub(",", ".", .x)))) %>%
      tidyr::pivot_longer(cols = c(2:63), names_to="Year", values_to = "values") %>%
      dplyr::filter(Year==year) %>%
      dplyr::top_n(n*pos)
    if (pos==-1){return(final %>% dplyr::arrange(values))}else{return(final %>% dplyr::arrange(desc(values)))}
  }
}



#' All Indicators like String
#' @description This function returns all indicators that contain string provided.
#' @param like A string the indicator name should contain.
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct filter collect
#' @export
#' @return A vector
#' @details This function returns all indicators that contain strings provided
#' @examples
#' # See all indicators that contain health in them
#' all_indicators_like("health")
#' @author Michael Mbajwa
all_indicators_like <- function(like){
  if (missing(like)){
    stop("like parameter must be provided.")
  }
  # read the data
  all_data <- FinalCompleteData
  indicators_like <- all_data %>% dplyr::select(Indicator_Name) %>% dplyr::distinct() %>% dplyr::filter(str_detect(Indicator_Name, paste("(?i)", like, sep=""))) %>% collect() %>% as.matrix %>% as.vector()
  if(length(indicators_like)>0){return(indicators_like)} else{
    options(warn = 1)
    warning(sprintf("No indicator like %s found", like))
  }
}



#' Plot all countries
#' @description This function returns a plot of all the countries we have indicators for.
#' @param map_title A string containing desired title for the plot
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct filter collect mutate
#' @importFrom maps world
#' @importFrom rnaturalearth ne_countries
#' @importFrom ggplot2 ggplot
#' @export
#' @return A plot
#' @details This function returns a plot of all the countries we have indicators for.
#' @examples
#' make_plot_countries()
#' @author Michael Mbajwa
make_plot_countries <- function(map_title="World Map"){
  if(!is.character(map_title)){stop("map_title must be a string.")}

  all_data_countries = all_countries()

  world <- ne_countries(scale = "medium", returnclass = "sf")

  world_modified <- world %>%
    mutate(my_selection = ifelse(admin %in% all_data_countries,
                                 1, NA))

  ggplot(data = world_modified) +
    geom_sf(aes(fill=my_selection)) +
    theme_void() +
    theme(legend.position = "none") + labs(title=map_title)
}



#' Country map
#' @description This function returns a plot of specified country with key details provided.
#' @param all_country_details A dataframe returned from the function country_key_details("nigeira").
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct filter collect mutate
#' @importFrom maps world
#' @importFrom rnaturalearth ne_countries
#' @importFrom ggplot2 ggplot map_data theme_bw theme geom_polygon
#' @export
#' @return A ggplot
#' @details This function returns a plot of specified country with key details provided.
#' @examples
#' # See all indicators that contain health in them
#' map_country(all_country_details=country_key_details("nigeria"))
#' @author Michael Mbajwa
map_country <- function(all_country_details){
  country_details <- all_country_details %>% pivot_wider(names_from = Columns, values_from = Values)
  country <- country_details$Country_Name

  currency <- paste("Currency:", country_details$Currency_Unit)
  region <- paste("Region:", country_details$Region)
  income_grp <- paste("Income Group:", country_details$Income_Group)
  head_state <- paste("Head of State:", country_details$Head_of_state)
  head_gov <- paste("Head of Government:", country_details$Head_of_government)
  sup_title <- paste(currency, region, income_grp, head_state, head_gov, sep="\n")

  if(!country %in% map_data('world')$region) stop(paste('Country name:', country, "not recognized"))

  ## Let's define our custom theme for the final map
  map_country_theme <- theme_bw() +
    theme(panel.background = element_rect(fill = '#4e91d2'),
          legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(color = "black", size = 12, face = "bold"),
          plot.subtitle = element_text(color = "blue", face = "italic"))

  ## make a df with only the country to overlap
  map_data_country <- map_data('world')[map_data('world')$region == country,]

  # get coordinates
  x_min <- min(map_data_country$long)
  x_max <- max(map_data_country$long)

  y_min <- min(map_data_country$lat)
  y_max <- max(map_data_country$lat)

  x_limits <- c(x_min-5, x_max+5)
  y_limits <- c(y_min, y_max)

  ## The map (maps + ggplot2 )
  ggplot() +
    ## First layer: worldwide map
    geom_polygon(data = map_data("world"),
                 aes(x=long, y=lat, group = group),
                 color = '#9c9c9c', fill = '#f3f3f3') +
    ## Second layer: Country map
    geom_polygon(data = map_data_country,
                 aes(x=long, y=lat, group = group),
                 color = '#4d696e', fill = '#8caeb4') +
    coord_map() +
    coord_fixed(1.3,
                xlim = x_limits,
                ylim = y_limits) +
    ggtitle(label=paste0("A map of ", stringr::str_to_upper(country)),
            subtitle = sup_title) +
    scale_x_continuous(n.breaks = 20) +
    scale_y_continuous(n.breaks = 20) +
    map_country_theme
}
