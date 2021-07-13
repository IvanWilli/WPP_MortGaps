############## Canada

# libraries
.packages = c("devtools", "data.table","DDSQLtools","DemoTools","jsonlite","openssl","tidyverse")
xfun::pkg_attach(.packages)
source("R/funs.R")

# countries
locs_tier1 <- read.csv("data/locs_tier1.csv") 

# country code and dates out
country_i = "Denmark"; dates_out = 1950.5:2020.5;

# get data
country_data <- read.csv(paste0("data/",country_i,".csv"))

# wich data is available
plot_data(country_data)
country_data %>% count(DataSourceShortName,IndicatorName)

# select best data: single & HMD
m_single <- country_data %>% filter(IndicatorName=="m(x,n) - complete",
                              SexName!="Both sexes",
                              DataReliabilitySort == min(country_data$DataReliabilitySort),
                              DataSourceShortName=="HMD")

# format and extend single
m_single <- m_single %>% 
     split(list(m_single$TimeLabel, m_single$SexName)) %>% 
     lapply(function(X){
          LT_sex <- ifelse(unique(X$SexName)=="Female","f","m")
          LT <- lt_single_mx(nMx = X$DataValue,
                             Age = X$AgeStart,
                             Sex = LT_sex,
                             OAnew = 100)
          LT$Sex <- LT_sex
          LT$Date <- unique(X$TimeMid)
          LT
     }) %>% 
     do.call(rbind,.)

# see input
plot_age_time(m_single)

# detect year gaps and split them
years_gap <- sort(dates_out[which(!dates_out %in% unique(m_single$Date))])
intervals_gap <- split(years_gap, cumsum(c(1, diff(years_gap) != 1)))

