############## Germany: gaps in nMx

# libraries
.packages = c("devtools", "data.table","DDSQLtools","DemoTools","jsonlite","openssl","tidyverse")
lapply(.packages, require, character.only=TRUE)
source("R/funs.R")

# country code and dates out
country_i = 372; dates_out = 1950.5:2020.5;

# get data
# m_data <- get_data(country = country_i, years = range(dates_out))
m_data <- read.csv("data/ireland.csv")

# wich data is available
plot_data(m_data)

# select best data: single & HMD
m_single <- m_data %>% filter(IndicatorName=="m(x,n) - complete",
                              SexName!="Both sexes",
                              DataReliabilitySort == min(m_data$DataReliabilitySort),
                              DataSourceShortName=="HMD")

# an issue: zero rates 
m_single %>% filter(DataValue==0) %>% count(TimeLabel,AgeStart)

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

# option 1: Use abr lt - 
        # I have only 3 radixes in EuroStat
        # I have e(x) only from DYB
        # use WPP?
range_gap <- range(intervals_gap[[1]])
data_alternatives <-m_data %>% filter(TimeMid>=min(intervals_gap[[1]]),
                                     TimeMid<=max(intervals_gap[[1]]+10)) %>% 
                              count(IndicatorName, DataSourceShortName, TimeLabel)

m_abr <- m_data %>% filter(IndicatorName=="m(x,n) - abridged",
                           SexName!="Both sexes",
                           TimeMid >= range_gap[1],
                           DataSourceShortName=="WPP")


##################################################################################

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

window <- 30
m_gap2_2 <-rbind(m_single, m_gap2) %>% 
                filter(Date %in% (range_gap[1]-window):2020)

# assign the mean moving average 2
m_gap2_2 <- m_gap2_2 %>% mutate(nMx_mean = zoo::rollmean(nMx,k=3,fill=NA),
                            nMx = ifelse(nMx==0,nMx_mean,nMx))
debugonce(lc)
m_gap2_2 <- lc(input = m_gap2_2, dates_out = 2020)
m_gap2_2 <- m_gap2_2 %>% 
        split(list(m_gap2_2$Date, m_gap2_2$Sex)) %>% 
        lapply(function(X){
                LT_sex <- unique(X$Sex)
                LT <- lt_single_mx(nMx = X$nMx,
                                         Age = X$Age,
                                         Sex = LT_sex,
                                         OAnew = 100)
                LT$Sex <- LT_sex
                LT$Date <- unique(X$Date)
                LT
        }) %>% 
        do.call(rbind,.)

# final
m_final <- rbind(m_single,m_gap2 %>% select(-nMx_mean),m_gap2_2)

# plot it: seems better
plot_age_time(m_final)
