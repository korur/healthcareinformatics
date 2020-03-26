# Libraries required

library(tidyverse)
library(treemap) 
library(d3treeR)
library(htmlwidgets)

# Set Functions needed for data preparation

# Rename
#' 
#' Rename first few columns
#' 
#' @param df Sheet.
#' 
#' @keywords internal
rename_sheets <- function(df){
  names(df)[1:4] <- c(
    "state",
    "country",
    "lat", 
    "lon"
  )
  return(df)
}

#' Pivot
#' 
#' Change data from wide to long.
#' 
#' @param df Sheet.
#' 
#' @keywords internal
pivot <- function(df){
  tidyr::pivot_longer(
    df, 
    tidyselect::contains("/"),
    names_to = c("date"),
    values_to = c("cases"),
    values_ptypes = list(cases = "character")
  )
}

#' Convert
#' 
#' Convert dates.
#' 
#' @keywords internal
as_date <- function(date){
  date <- lubridate::mdy(date, "%m/%d/%Y")
  date[!is.na(date)]
}

####  Prepare the Data  ####

# jhu data
confirmed_sheet <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
deaths_sheet <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

# confirmed cases
confirmed <- readr::read_csv(confirmed_sheet, col_types = readr::cols())
# deaths
deaths <- readr::read_csv(deaths_sheet, col_types = readr::cols()) 

# add col
confirmed$type <- "confirmed"
deaths$type <- "death"

# rename
confirmed <- rename_sheets(confirmed)
deaths <- rename_sheets(deaths)  

# pivot longer
confirmed <- pivot(confirmed)
deaths <- pivot(deaths)    

suppressWarnings({    
  df <- dplyr::bind_rows(confirmed, deaths) %>% 
    dplyr::mutate(
      date = as_date(date),
      cases = trimws(cases),
      cases = as.numeric(cases),
      cases = dplyr::case_when(
        is.na(cases) ~ 0,
        TRUE ~ cases
      ),
      country_iso2c = countrycode::countrycode(country, "country.name", "iso2c")
    )
})
df$state <- ifelse(is.na(df$state), df$country,df$state)

# Make a Treemap


# Group countries 
df3 <- df %>% filter(date==max(date))
df4 <- df3 %>% group_by(country, type) %>% summarise(n=sum(cases)) 

# Correct for some mistakes in the data ( negative values)
df4$n <- ifelse(df4$n < 0, 0, df4$n )

# Prepare the tree
df4$type2 <- paste(df4$type, ": ", df4$n )

mytree <- treemap(df4,
                  index=c("country", "type2"),
                  vSize="n",
                  vColor="country")

# Custom Style for the TreemapD3

# Use style_widget fcuntion to change the style from https://github.com/d3treeR/d3treeR/issues/10#issuecomment-248098578

style_widget <- function(hw=NULL, style="", addl_selector="") {
  stopifnot(!is.null(hw), inherits(hw, "htmlwidget"))
  
  # use current id of htmlwidget if already specified
  elementId <- hw$elementId
  if(is.null(elementId)) {
    # borrow htmlwidgets unique id creator
    elementId <- sprintf(
      'htmlwidget-%s',
      htmlwidgets:::createWidgetId()
    )
    hw$elementId <- elementId
  }
  
  htmlwidgets::prependContent(
    hw,
    htmltools::tags$style(
      sprintf(
        "#%s %s {%s}",
        elementId,
        addl_selector,
        style
      )
    )
  )
}

# Draw the final map
cw <- style_widget(
  d3tree(mytree, rootname = "Distribution of Coronavirus Cases in the World - Created by www.dataatomic.com"),
  addl_selector="text",
  style="font-family:roboto; font-size:13px;"
)

cw

# Saving the widget

htmlwidgets::saveWidget(cw, "covidtreemap.html")

