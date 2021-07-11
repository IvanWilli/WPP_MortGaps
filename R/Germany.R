############## Germany: gaps in nMx

# libraries
.packages = c("devtools", "data.table","DDSQLtools","DemoTools","jsonlite","openssl","tidyverse")
lapply(.packages, require, character.only=TRUE)
source("R/funs.R")

# country code and dates out
country_i = 276; dates_out = 1950:2020;

# get data
# m_data <- get_data(country = country_i, years = c(min(dates_out)-10,max(dates_out)))
m_data <- read.csv("data/m_data_germany.csv")

# wich data is available
m_data_avail <- m_data %>% count(IndicatorName, TimeLabel) %>% 
                        pivot_wider(names_from=IndicatorName,
                                    values_from=n) %>% 
                        as.data.frame() %>% arrange(TimeLabel)

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
                                           extrapFrom = 100, 
                                           OAnew = 105)
                        LT$Sex <- LT_sex
                        LT$Date <- unique(X$TimeLabel)
                        LT
                }) %>% 
                do.call(rbind,.)

# see input
plot_age_time(m_single)

# detect year gaps and split them
years_gap <- sort(dates_out[which(!dates_out %in% unique(m_single$Date))])
intervals_gap <- split(years_gap, cumsum(c(1, diff(years_gap) != 1)))

### gap 1: from 1950 to 1955
range_gap <- range(intervals_gap[[1]])

# extra data to use to model lt? 
data_alternatives <-m_data %>% filter(TimeMid>min(intervals_gap[[1]]-10),
                                      TimeMid<max(intervals_gap[[1]])) %>% 
                                count(IndicatorName, TimeLabel, DataSourceShortName)

# option 1: an abr life table as pivot and apply LC lim - from HLD
m_abr <- m_data %>% filter(IndicatorName=="m(x,n) - abridged",
                           TimeLabel == "1946-1947")

# handle ages 0, 1 and 2. To perform! - work lx! detect function.
m_abr$DataValue[m_abr$AgeStart==1 & m_abr$SexName=="Female"] <- 
        (m_abr$DataValue[m_abr$AgeStart==1 & m_abr$SexName=="Female"] + 
         3 * m_abr$DataValue[m_abr$AgeStart==2 & m_abr$SexName=="Female"])/4
m_abr$DataValue[m_abr$AgeStart==1 & m_abr$SexName=="Male"] <- 
        (m_abr$DataValue[m_abr$AgeStart==1 & m_abr$SexName=="Male"] + 
                 3 * m_abr$DataValue[m_abr$AgeStart==2 & m_abr$SexName=="Male"])/4
m_abr <- subset(m_abr,AgeStart!=2)

# make it simple
m_abr_single <- m_abr %>% 
        split(list(m_abr$TimeMid, m_abr$SexName)) %>% 
        lapply(function(X){
                LT_sex <- ifelse(unique(X$SexName)=="Female","f","m")
                LT <- lt_abridged2single(nMx = X$DataValue,
                                         Age = X$AgeStart,
                                         Sex = LT_sex,
                                         extrapFrom = 90, 
                                         OAnew = 105)
                LT$Sex <- LT_sex
                LT$Date <- unique(X$TimeMid)
                LT
        }) %>% 
        do.call(rbind,.)

# apply LC lim with time points: 1947, 1956 and 1965 - apply to last observed!
window <- 2 # important because of assumption on k(t)
m_gap1 <- rbind(m_abr_single %>% select(Date,Sex,Age,nMx),
                m_single %>% 
                        select(Date,Sex,Age,nMx) %>% 
                        filter(Date %in% c(range_gap[2]+1,
                                           range_gap[2]+window))) %>% 
                mutate(Date = as.numeric(Date))
m_gap1 <- interp_lc_lim(input = m_gap1, 
                        dates_out = as.numeric(intervals_gap[[1]]), Single = T)
m_final <- rbind(m_single, m_gap1$lt_hat)

# plot it
plot_age_time(m_final)

# life expectancy much lower, maybe not good idea
plot(m_single$Date[m_single$Age%in%c(0,65) & m_single$Date%in%c(1956:1990)],
     m_single$ex[m_single$Age%in%c(0,65) & m_single$Date%in%c(1956:1990)],
     xlim=c(1940,2020),xlab="age",ylab="Date",main="e0 and e65. Germany")
points(m_abr_single$Date[m_abr_single$Age%in%c(0,65)],
       m_abr_single$ex[m_abr_single$Age%in%c(0,65)],col=2)
legend("bottomright",c("single","abr"),pch=1,col=1:2)


# option 2 in gap 1: extrapolate with usual LC
window <- 30
m_gap1 <- rbind(m_single %>% 
                        select(Date,Sex,Age,nMx) %>% 
                        filter(Date %in% c((range_gap[2]+1):(range_gap[2]+window)))) %>% 
                mutate(Date = as.numeric(Date))
m_gap1 <- lc(input = m_gap1, dates_out = as.numeric(intervals_gap[[1]]))
m_gap1 <- m_gap1 %>% 
        split(list(m_gap1$Date, m_gap1$Sex)) %>% 
        lapply(function(X){
                LT <- lt_single_mx(nMx = X$nMx,
                                   Age = X$Age,
                                   Sex = unique(X$Sex),
                                   extrapFrom = 100, 
                                   OAnew = 105)
                LT$Sex <- unique(X$Sex)
                LT$Date <- unique(unique(X$Date))
                LT
        }) %>% 
        do.call(rbind,.)
m_final <- rbind(m_single, m_gap1)

# plot it: seems better
plot_age_time(m_final)

### gap 2: do I have extra data
range_gap <- range(intervals_gap[[2]])
data_alternatives <-m_data %>% filter(TimeMid>min(intervals_gap[[2]]+10),
                                      TimeMid<max(intervals_gap[[2]])) %>% 
                                count(IndicatorName, TimeLabel)

# option 1: LC extrapolation
window <- 30
m_gap2 <-m_single %>% select(Date,Sex,Age,nMx) %>% 
                        filter(Date %in% (range_gap[1]-window):(range_gap[1]-1)) %>% 
         mutate(Date = as.numeric(Date))

m_gap2 <- lc(input = m_gap2, dates_out = as.numeric(intervals_gap[[2]]))
m_gap2 <- m_gap2 %>% 
        split(list(m_gap2$Date, m_gap2$Sex)) %>% 
        lapply(function(X){
                LT <- lt_single_mx(nMx = X$nMx,
                                   Age = X$Age,
                                   Sex = unique(X$Sex),
                                   extrapFrom = 100, 
                                   OAnew = 105)
                LT$Sex <- unique(X$Sex)
                LT$Date <- unique(unique(X$Date))
                LT
        }) %>% 
        do.call(rbind,.)
# final
m_final <- rbind(m_single,m_gap1,m_gap2)

# graph
plot_age_time(m_final)

# option 2: Use abr lt
m_abr <- m_data %>% filter(IndicatorName=="m(x,n) - abridged",
                           TimeMid >= range_gap[1])

# make it simple
m_gap2 <- m_abr %>% 
        split(list(m_abr$TimeMid, m_abr$SexName)) %>% 
        lapply(function(X){
                LT_sex <- ifelse(unique(X$SexName)=="Female","f","m")
                LT <- lt_abridged2single(nMx = X$DataValue,
                                         Age = X$AgeStart,
                                         Sex = LT_sex,
                                         # extrapFrom = 100, 
                                         OAnew = 100)
                LT$Sex <- LT_sex
                LT$Date <- unique(X$TimeMid)
                LT
        }) %>% 
        do.call(rbind,.)

# final
m_final <- rbind(m_gap1, m_single, m_gap2)

# plot it: seems better
plot_age_time(m_final)

######################## general items
# changing slope on rate decreasing before last observation makes LC lim not fully suitable. Test other methods
# cohort effects are not considered
# test consistency between siurces
# fit e0 in lc and lc lim
# excess mort in 2020
# I have to bring ax from sources


### 100 as OAnew! take a look
### demography: lc fit
### parameters to apply in gaps
### soviet union: use wpp 1950 baseline but...

