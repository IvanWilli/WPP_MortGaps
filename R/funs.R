# additional funs --------------------------------------------------------------

lc <- function(input, dates_out){
     dates_in = sort(unique(as.numeric(input$Date)))
     input %>% split(input$Sex) %>% 
          lapply(function(X){
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

# diagnostic gaps
# years_gap <- sort(dates_out[which(!dates_out %in% unique(m_single$TimeLabel))])
# intervals_gap <- split(years_gap, cumsum(c(1, diff(years_gap) != 1)))
# stopifnot(length(years_gap)!=0)

# rule for each gap (try from right to left next)
# for(gap in intervals_gap){
#      # gaps in starting period; gap = intervals_gap[[1]]
#      if(1950 %in% gap){
#           if(length(gap)>=5){
#                m_1950 <- get_m_1950(m_data, gap)
#                # create a pivot in 1950 and use LC lim     
#           }else{
#                # apply lc lim
#           }
#           
#      }
#      # gaps finishing period
#      if(2020 %in% gap){
#           if(length(gap)<=5){
#                # use LC with previos years (until previous gap) and forecast only 5     
#           }else{
#                stop("sorry...")
#           }
#      }
#      # gaps in the middle
#      if(2020 %in% gap){
#           if(length(gap)<=10){
#                # interpolate
#           }else{
#                # use relational method
#           }
#      }
# }

# design out

# diagnostics outs

# out


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
get_data <- function(country = NULL, 
                     indicatorIds = c(255,256,234,239,272,245,246),years){
 
# get_iitypes() %>% 
#         filter(PK_IndicatorID %in% c(255,256,234,239,272,245,246))

options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")
     out <- get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),
                           startYear = min(floor(years)),
                           endYear = max(floor(years))+1,
                           indicatorIds = indicatorIds,
                           locIds = country,
                           locAreaTypeIds = 2,     ## "Whole area"
                           subGroupIds = 2,        ## "Total or All groups"
                           includeUncertainty = FALSE,
                           collapse_id_name = FALSE) %>% 
          as.data.table() %>% deduplicates(.)
     out
     out[, .(IndicatorName, DataSourceShortName, DataReliabilitySort,
             SexName, AgeStart, AgeSpan, 
             TimeLabel, TimeMid, TimeDuration, DataValue)]
}

# see available data
plot_data <- function(data){
        data$IndicatorName <- factor(data$IndicatorName)
        dodge <- position_dodge(width=0.4)
        data %>% distinct(IndicatorName,DataSourceShortName,TimeMid) %>% 
                ggplot(aes(x=TimeMid,y=DataSourceShortName)) +
                geom_point(aes(color=IndicatorName,shape=IndicatorName),position = dodge)+
                scale_shape_manual(values=1:nlevels(data$IndicatorName)) +
                scale_x_continuous(name ="Year",
                                   limits = c(1940,2020),
                                   breaks = seq(1940,2020,10), 
                                   labels = seq(1940,2020,10))+
                ggtitle(paste0(data$LocName[1],". Available Data"))+
                geom_vline(xintercept = c(1950,2020),linetype=2)+
                theme_bw()
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
     data$Age[data$Age>100] <- 100 
     data$Age_col <- cut(data$Age,breaks = 10)
     data$Date <- as.numeric(data$Date)
     minim_data <- floor(min(data$Date)/10)*10
     ggplot(data) + 
          geom_line(aes(x=as.numeric(Date),y=nMx,
                        group=factor(Age),
                        col=factor(Age_col))) +
          geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
          geom_vline(xintercept = range(data$Date),linetype=2,col=2)+
          scale_x_continuous(name ="Year",
                             breaks = seq(minim_data,2020,10), 
                             labels = seq(minim_data,2020,10))+
          scale_y_log10()+
          scale_fill_continuous(name = "New Legend Title")+
          theme_bw()+
          facet_grid(~Sex)
}

