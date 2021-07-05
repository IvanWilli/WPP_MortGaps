############## Germany: gaps in nMx

# libraries
.packages = c("devtools", "data.table","DDSQLtools","DemoTools","jsonlite","openssl","tidyverse")
lapply(.packages, require, character.only=TRUE)

# country code and dates out
country_i = 276; dates_out = 1950:2020;

# get data
m_data <- get_data(country = country_i, years = c(min(dates_out)-10,max(dates_out)))

# wich data is available
m_data_avail <- m_data %>% count(IndicatorName, TimeLabel) %>% 
                        pivot_wider(names_from=IndicatorName,
                                    values_from=n) %>% 
                        as.data.frame()

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

# gap 1: from 1950 to 1955
range_gap <- range(intervals_gap[[1]])

# extra data to use to model lt? 
data_alternatives <-m_data %>% filter(TimeMid>min(intervals_gap[[1]]-10),
                                   TimeMid<max(intervals_gap[[1]])) %>% 
                                count(IndicatorName, TimeLabel)

# option 1: an abr life table as pivot and apply LC lim - from HLD
m_abr <- m_data %>% filter(IndicatorName=="m(x,n) - abridged",
                           TimeLabel == "1946-1947",
                           AgeStart!=2) # take care of this later

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

# apply LC lim
window <- 10
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

# plot it: seems better. cut cohort effect
plot_age_time(m_final)

# gap 2: do I have extra data
range_gap <- range(intervals_gap[[2]])
data_alternatives <-m_data %>% filter(TimeMid>min(intervals_gap[[2]]+10),
                                      TimeMid<max(intervals_gap[[2]])) %>% 
                                count(IndicatorName, TimeLabel)

# LC
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










# additional funs --------------------------------------------------------------

lc <- function(input, dates_out){
        dates_in = sort(unique(input$Date))
        input %>% split(input$Sex) %>% 
                lapply(function(X){
                        browser()
                        M <- X %>% select(Date,nMx,Age) %>% 
                                pivot_wider(names_from = Date, values_from = nMx)   %>% 
                                select(-Age) %>% as.matrix()
                        ndates_in <- ncol(M)
                        ax  <- rowSums(log(M))/ndates_in
                        M_svd      <- svd(log(M)-ax)
                        bx         <- M_svd$u[, 1]/sum(M_svd$u[, 1])
                        kt         <- M_svd$d[1] * M_svd$v[, 1] * sum(M_svd$u[, 1])  
                        kt_diff <- diff(kt)
                        summary_kt <- summary(lm(kt_diff ~ 1))
                        kt_drift <- summary_kt$coefficients[1,1]
                        h <- dates_out - dates_in[1]
                        kt_forecast <- head(kt, 1) + (h * kt_drift)
                        M_hat <- exp(ax + sapply(kt_forecast,FUN = function(k) bx * k)) %>% as.data.frame()
                        colnames(M_hat) <- dates_out
                        M_hat$Age <- sort(unique(X$Age))
                        M_hat <- M_hat %>%
                                pivot_longer(cols=-ncol(M_hat), names_to="Date",values_to="nMx")
                        M_hat$Sex = unique(X$Sex)
                        M_hat
                        
                }) %>% 
                do.call("rbind", .)
}
        
# function that fill gaps in mortality time serie
# fill_gaps <- function(country = 276, dates_out = 1950:2020){
     
     # get data using UN server
     m_data <- get_data(country = country, years = c(min(dates_out)-10,max(dates_out)))
     
     # use sngle and most relieable data
     m_single <- m_data %>% filter(IndicatorName=="m(x,n) - complete",
                              SexName != "Both sexes",
                              DataReliabilitySort == min(m$DataReliabilitySort)) %>% 
                       filter(SexName == "Female")
     
     # diagnostic gaps
     years_gap <- sort(dates_out[which(!dates_out %in% unique(m_single$TimeLabel))])
     intervals_gap <- split(years_gap, cumsum(c(1, diff(years_gap) != 1)))
     stopifnot(length(years_gap)!=0)
     
     # rule for each gap (try from right to left next)
     for(gap in intervals_gap){
          # gaps in starting period; gap = intervals_gap[[1]]
          if(1950 %in% gap){
               if(length(gap)>=5){
                    m_1950 <- get_m_1950(m_data, gap)
                         # create a pivot in 1950 and use LC lim     
                }else{
                        # apply lc lim
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

# get a lt near 1950 to interpolate
get_m_1950 <- function(m_data, gap){
        m_abr <- m_data %>% filter(IndicatorName=="m(x,n) - abridged", TimeMid<=max(gap), TimeMid>=1945)
        if(nrow(m_abr)!=0){
                # get abr lt
                m_abr <- m_data %>% filter(IndicatorName=="m(x,n) - abridged", TimeMid<=max(gap), TimeMid>=1945, SexName == "Female")
                m_abr_single <- lt_abridged2single(nMx = m_abr$DataValue, Age = m_abr$AgeStart,Sex = "f") 
                m_pivot_1950 <- m_abr_single
        }
        if(nrow(m_abr)==0){
                # get a lt model in 1950
                DemoTools::lt_model_lq(Sex = "f", q0_5 = 0.05, e0 = 65)
                demogR::cdmltw()
        }
}

# function to get data from WPP server
get_data <- function(country = NULL, indicatorIds = c(196,234,239,272,245,246), myDS = NULL, years){
        
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
        out
        out[, .(IndicatorName, DataSourceShortName, DataReliabilitySort,
                SexName, AgeStart, AgeSpan, 
                TimeLabel, TimeMid, TimeDuration, DataValue)]
}



## de-duplicate function to rank and filter series based on different set of criteria (function took from PG)
deduplicates <- function(myDT) {
        ## Filter out Model-based estimates from IDB and GBD
        ## but keep "model-based estimates" from VR computed for WPP using time interpolation to fill-in data gaps
        myDT <- myDT[(! DataTypeID %in% c(70, 71)) | (DataTypeID %in% c(70) & DataSourceShortName=="WPP")]
        ## for HMD, keep only annual series
        myDT <- myDT[(DataSourceShortName!="HMD") | (DataSourceShortName=="HMD" & TimeDuration==1)]
        
        ## sort records per location and year to order multiple observation by multi-criteria using sort orders
        setorder(myDT, LocID, TimeMid, DataCatalogShortName,
                 StatisticalConceptSort,
                 DataStatusSort,
                 DataProcessSort, DataProcessTypeSort,
                 DataReliabilitySort,
                 -DataSourceYear, DataSourceSort, DataSourceShortName,
                 -DataTypeSort,
                 ModelPatternName, PeriodGroupName, PeriodStart, PeriodSpan,
                 SexSort, AgeStart, AgeSpan)
        
        ## subset key attributes to rank most authoritative series
        ## mySeries <- unique(myDT[, .(SeriesID, LocID, DataCatalogShortName, TimeMid, DataSourceShortName, DataSourceYear, DataSourceSort, DataStatusName, DataStatusSort, DataProcessSort, DataProcessTypeSort, StatisticalConceptName, StatisticalConceptSort, DataTypeName, DataTypeSort, DataReliabilityName, DataReliabilitySort)])
        myDT[, MD5 := md5(paste(LocID, DataCatalogShortName, TimeMid, DataSourceShortName, DataSourceYear, DataSourceSort, DataStatusName, DataStatusSort, DataProcessSort, DataProcessTypeSort, StatisticalConceptName, StatisticalConceptSort, DataTypeName, DataTypeSort, DataReliabilityName, DataReliabilitySort, sep="-"))]
        mySeries <- unique(myDT[, .(MD5, LocID, DataCatalogShortName, TimeMid, DataSourceShortName, DataSourceYear, DataSourceSort, DataStatusName, DataStatusSort, DataProcessSort, DataProcessTypeSort, StatisticalConceptName, StatisticalConceptSort, DataTypeName, DataTypeSort, DataReliabilityName, DataReliabilitySort)])
        
        setorder(mySeries, LocID, DataCatalogShortName,
                 StatisticalConceptSort,
                 DataStatusSort,
                 DataProcessSort, DataProcessTypeSort,
                 DataReliabilitySort,
                 -DataSourceYear, DataSourceSort, DataSourceShortName,
                 -DataTypeSort)
        
        ## assign rank to each set of "dups" (from different sources, etc.)
        mySeries[, nrank1 := 1:.N, by=list(LocID, DataCatalogShortName, DataProcessSort, DataTypeSort, trunc(TimeMid))]
        ## for estimates and VR
        mySeries[, nrank2 := 1:.N, by=list(LocID, DataCatalogShortName, DataProcessSort, trunc(TimeMid))]
        ## in case only 1 series get used
        mySeries[, nrank3 := 1:.N, by=list(LocID, DataCatalogShortName, trunc(TimeMid))]
        
        ## keep only most authoritative versions (top #1) based on DataCatalogShortName, DataProcessSort, DataTypeSort
        myDT <- merge(myDT, mySeries[, .(MD5, nrank1, nrank2, nrank3)], by=c("MD5"))
        ## alternative use nrank3 instead of nrank2 if we want to always pick Estimates > Register when both are available
        myDT <- myDT[(!DataProcessTypeID %in% c(6, 9) & nrank1==1) | (DataProcessTypeID %in% c(6, 9) & nrank2==1)]
        myDT$MD5 <- NULL
        myDT$nrank1 <- NULL
        myDT$nrank2 <- NULL
        
        return(myDT)
}

plot_age_time <- function(data){
        data$Age_col <- cut(data$Age,breaks = 10) 
        ggplot(data) + 
                geom_line(aes(x=as.numeric(Date),y=nMx,
                              group=factor(Age),
                              col=factor(Age_col))) +
                geom_vline(xintercept = c(1956,2017),linetype=2)+
                scale_x_continuous(name ="Year", 
                                 breaks = seq(1950,2020,10), labels = seq(1950,2020,10))+
                scale_y_log10()+
                theme_bw()+
                facet_grid(~Sex)
}
