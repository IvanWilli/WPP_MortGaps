library(devtools)
library(DDSQLtools)
library(DemoTools)
library(tidyverse)

# function that fill gaps in mortality time serie
fill_gaps <- function(country, dates_out = 1950:2020){
     
     # get data using UN server
     m <- get_data(country = country, years = c(min(dates_out)-10,max(dates_out)))
     
     # use sngle and most relieable data
     m_single <- m %>% filter(IndicatorName=="m(x,n) - complete",
                              SexName != "Both sexes",
                              DataReliabilitySort == min(m$DataReliabilitySort))
     
     # diagnostic gaps
     m_single <- m_single %>% filter(SexName == "Female")
     years_gap <- sort(dates_out[which(!dates_out %in% unique(m_single$TimeLabel))])
     intervals_gap <- split(years_gap, cumsum(c(1, diff(years_gap) != 1)))
     stopifnot(length(years_gap)==0,"Nothing to fill.")
     
     # rule for each gap (try from right to left next)
     for(gap in intervals_gap){
          # gaps in starting period
          if(1950 %in% gap){
               if(length(gap)>=5){
                    if(there_is_a_pivot_in_1940s_with_abr){
                         # turn complete the abrs and use LC lim
                    }else{
                         # create a pivot in 1950 and use LC lim     
                    }
               }
          }
          # gaps finishing period
          if(2020 %in% gap){
               if(length(gap)<=5){
                    # use LC with previos years (until previous gap) and forecast only 5     
               }else{
                    stop("sorry...")
               }
          }
          # gaps in the middle
          if(2020 %in% gap){
               if(length(gap)<=10){
                    # interpolate
               }else{
                    # use relational method
               }
          }
     }
          
     # design out
     
     # diagnostics outs
     
     # out
}



# function to get data from WPP server
get_data <- function(country = 276, indicatorIds = c(245,246), myDS = "DYB", years){
     out <- get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),
                           startYear = min(floor(years)),
                           endYear = max(floor(years))+1,
                           indicatorIds = indicatorIds,
                           locIds = country,
                           locAreaTypeIds = 2,     ## "Whole area"
                           subGroupIds = 2,        ## "Total or All groups"
                           dataSourceShortNames = myDS,
                           includeUncertainty = FALSE,
                           collapse_id_name = FALSE) %>% 
           as.data.table() %>% deduplicates(.)
     out[, .(IndicatorName, DataReliabilitySort, SexName, AgeStart, AgeSpan, 
             TimeLabel, TimeMid, TimeDuration, DataValue)]
}

