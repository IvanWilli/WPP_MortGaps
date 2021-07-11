############## Canada

# libraries
.packages = c("devtools", "data.table","DDSQLtools","DemoTools","jsonlite","openssl","tidyverse")
lapply(.packages, require, character.only=TRUE)
source("R/funs.R")

# country code and dates out
country_i = 124; dates_out = 1950.5:2020.5;

# get data
# m_data <- get_data(country = country_i, years = c(min(dates_out)-10,max(dates_out)))
m_data <- read.csv("data/m_data_canada.csv")

# wich data is available
plot_data(m_data)

# select best data: single & HMD
m_single <- m_data %>% filter(IndicatorName=="m(x,n) - complete",
                              SexName!="Both sexes",
                              DataReliabilitySort == min(m_data$DataReliabilitySort),
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

### gap: do I have extra data?
range_gap <- range(intervals_gap[[1]])
data_alternatives <-m_data %>% filter(TimeMid>=min(intervals_gap[[1]]-.5),
                                      TimeMid<=max(intervals_gap[[1]]+10)) %>% 
                                count(IndicatorName, DataSourceShortName, 
                                      TimeLabel, TimeMid)

# should I use them?
# option 1: Use abr lt
m_abr <- m_data %>% filter(IndicatorName=="m(x,n) - abridged",
                           SexName!="Both sexes",
                           TimeMid >= range_gap[1])

# make it simple
m_gap2 <- m_abr %>% 
        split(list(m_abr$TimeMid, m_abr$SexName)) %>% 
        lapply(function(X){
                LT_sex <- ifelse(unique(X$SexName)=="Female","f","m")
                LT <- lt_abridged2single(nMx = X$DataValue,
                                         Age = X$AgeStart,
                                         Sex = LT_sex,
                                         OAnew = 100)
                LT$Sex <- LT_sex
                LT$Date <- unique(X$TimeMid)
                LT
        }) %>% 
        do.call(rbind,.)

# bind with hmd data
window <- last(m_gap2$Date):c(last(m_gap2$Date)-30)
m_gap2 <- rbind(m_single, m_gap2) %>% 
        select(Date,Sex,Age,nMx) %>% 
        filter(Date >= min(window),Date <= max(window)) %>% 
        mutate(Date = as.numeric(Date))

# appply lc
table(m_gap2$Date)
m_gap2_2 <- lc(input = m_gap2, dates_out = 2019.5:2020.5)

# final
m_final <- rbind(m_single %>% select(Date,Sex,Age,nMx) ,m_gap2,m_gap2_2)

# plot it: seems better
plot_age_time(m_final)







