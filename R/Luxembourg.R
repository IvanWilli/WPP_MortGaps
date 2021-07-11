############## Germany: gaps in nMx

# libraries
.packages = c("devtools", "data.table","DDSQLtools","DemoTools","jsonlite","openssl","tidyverse")
lapply(.packages, require, character.only=TRUE)
source("R/funs.R")

# country code and dates out
country_i = 442; dates_out = 1950.5:2020.5;

# get data
# m_data <- get_data(country = country_i, years = c(min(dates_out)-10,max(dates_out)))
m_data <- read.csv("data/luxembourg.csv")

# wich data is available
plot_data(m_data)

# select best data: single & HMD
m_single <- m_data %>% filter(IndicatorName=="m(x,n) - complete",
                              SexName!="Both sexes",
                              DataReliabilitySort == min(m_data$DataReliabilitySort),
                              DataSourceShortName=="HMD")

# an issue: zero rates - around 600k pop
m_single %>% filter(DataValue==0) %>%  count(TimeLabel,SexName) %>% as.data.table()

# a year
m_single %>% filter(TimeLabel=="1960",DataValue==0) %>% as.data.frame()

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

### gap 1: from 1950 to 1959
range_gap <- range(intervals_gap[[1]])

# extra data to use to model lt? 
data_alternatives <-m_data %>% filter(TimeMid>min(intervals_gap[[1]]-10),
                                      TimeMid<max(intervals_gap[[1]])) %>% 
        count(IndicatorName, TimeLabel, TimeMid, DataSourceShortName) %>% 
        as.data.frame()

# option 1: an abr life table as pivot and apply LC lim - from HLD
m_abr <- m_data %>% filter(IndicatorName=="l(x) - complete",
                           TimeMid>min(intervals_gap[[1]]-10),
                           TimeMid<=max(intervals_gap[[1]]))

# make it simple
m_abr_single <- m_abr %>% 
        split(list(m_abr$TimeMid, m_abr$SexName)) %>% 
        lapply(function(X){
                LT_sex <- ifelse(unique(X$SexName)=="Female","f","m")
                X$DataValue <- lt_id_l_q(X$DataValue)
                LT <- lt_single_qx(nqx  = X$DataValue,
                                         Age = X$AgeStart,
                                         Sex = LT_sex,
                                         OAnew = 100)
                LT$Sex <- LT_sex
                LT$Date <- unique(X$TimeMid)
                LT
        }) %>% 
        do.call(rbind,.)
# final
m_final <- rbind(m_single, m_abr_single)

# plot it
plot_age_time(m_final)

### gap 2: do I have extra data? No
range_gap <- range(intervals_gap[[2]])
data_alternatives <-m_data %>% filter(TimeMid>=min(intervals_gap[[2]]),
                                      TimeMid<=max(intervals_gap[[2]]+10)) %>% 
                                count(IndicatorName, DataSourceShortName, TimeLabel)

# LC extrapolation - not used lc with automatic detection interval fit yet
window <- 30
m_gap2 <-m_single %>% select(Date,Sex,Age,nMx) %>% 
        filter(Date %in% (range_gap[1]-window):(range_gap[1]-1)) %>% 
        mutate(Date = as.numeric(Date))

# LC not accepts zeros: addditional constraints must be made to the log(M)
summary(m_gap2$nMx)
m_gap2 %>% filter(nMx==0) %>%  count(Date,Sex,Age) %>% as.data.table()
m_gap2 <- lc(input = m_gap2, dates_out = as.numeric(intervals_gap[[2]]))

# LC not accepts zeros
# moving average? not working either
m_gap2 <- m_gap2 %>% mutate(nMx_mean = zoo::rollmean(nMx,k=3,fill=NA),
                            nMx = ifelse(nMx==0,nMx_mean,nMx))
summary(m_gap2$nMx)

# for just 1 year: just extrapolate change on ratesor or take avg last 3 years? 
# that will be asjusted for covid effect

# smooth: filter null. Use interp function.