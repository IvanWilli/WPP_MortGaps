# additional funs --------------------------------------------------------------

# see available data
plot_data <- function(data){
        data$IndicatorName <- factor(data$IndicatorName)
        dodge <- position_dodge(width=0.5)
        data %>% distinct(IndicatorName,DataSourceShortName,TimeMid) %>% 
                ggplot(aes(x=TimeMid,y=DataSourceShortName)) +
                geom_point(aes(color=IndicatorName,shape=IndicatorName),position = dodge)+
                scale_shape_manual(values=1:nlevels(data$IndicatorName)) +
                scale_x_continuous(name ="Year",
                                   limits = c(1940,2020),
                                   breaks = seq(1940,2020,10), 
                                   labels = seq(1940,2020,10))+
                ggtitle("Available Data")+
                geom_vline(xintercept = c(1950,2020),linetype=2)+
                theme_bw()+ 
                theme(legend.position="bottom")+
                scale_fill_viridis_d()
}

# check data validation
is_data_valid <- function(country_data){
        validation <- country_data %>%
                split(list(.$IndicatorName, .$DataSourceShortName, 
                           .$SexName, .$TimeLabel), drop = T) %>% 
                lapply(function(X){
                # browser()
                # a lot of problems after that age 100 that we will smooth anyways
                validated <- coherence <- "not LT"
                abr_ages <- c(0,1,seq(5,60,5)) 
                # is a lifetable first, then wich one
                if(str_detect(X$IndicatorName[1],"l\\(x|m\\(x")){
                        X <- X %>% filter(AgeStart<=100)
                        Age <- X$AgeStart
                        AgeInt <- X$AgeSpan
                        AgeInt[AgeInt<1] <- NA
                        coherence <- suppressMessages(is_age_coherent(Age, AgeInt))
                        single_ok <- is_single(Age) & all(0:60 %in% Age)
                        abridg_ok <- is_abridged(Age[!Age %in% 2:4]) & all(abr_ages %in% Age) # to capture 0,1,2,4
                        neg_or_NA <- any(X$DataValue<0 | is.na(X$DataValue))
                        irregular <- sum(diff(X$DataValue)==0)>(length(Age)/5) # treshold of no risk
                        validated <- ifelse(neg_or_NA | irregular, "error", 
                                            ifelse(single_ok, "complete", 
                                                ifelse(abridg_ok,"abridged", 
                                                   "error")))
                }
                validation_i <- X %>% 
                                mutate(validation = validated, coherence = coherence)
                validation_i
                }
                ) %>% 
        do.call(rbind,.) %>% 
        remove_rownames(.) %>% 
                distinct()
        validation
}

# select data for each point
select_data <- function(country_data, exception = NULL){
        
        source_herarchy <- tibble(DataSourceShortName= c("HMD","EuroStat","HLD 2020","WPP","WHO DB","DYB","GBD 2016"),
                                  index = 1:7)
        country_data$TimeMid_floor <- floor(country_data$TimeMid) 
        
        time_data <- sort(unique(country_data$TimeMid_floor))
        
        for(year in time_data){
                # year = 1975
                selection <- country_data %>%
                        filter(TimeMid_floor == year, 
                               str_detect(IndicatorName,"x"),
                               !DataSourceShortName %in% exception) %>% 
                        group_by(DataSourceShortName, TimeMid_floor, TimeMid, TimeLabel, IndicatorName) %>% 
                        summarise(complete = is_single(unique(AgeStart)),
                                  has_age_2to4 = any(2:4 %in% AgeStart),
                                  OAG = max(AgeStart),
                                  is_avg_years = nchar(TimeLabel)) %>% 
                        left_join(source_herarchy,by = "DataSourceShortName") %>%
                        ungroup() %>% 
                        group_by(TimeMid_floor) %>% 
                        arrange(desc(complete),
                                index,
                                IndicatorName, # first lx
                                is_avg_years) %>% # first not avg years adta
                        slice(1)
                if(year == time_data[1]){
                        out <- selection
                }else{
                        out <- bind_rows(out, selection)
                }
        }
        out
}

# plot rates with time-age variation
plot_age_time <- function(data){
        data$Age[data$Age>100] <- 100 
        data$Age_col <- paste0(trunc(data$Age/10)*10,"-",trunc(data$Age/10)*10+10)
        data$Age_col[data$Age_col=="0-10"] <- "1-10"
        data$Age_col[data$Age==0] <- "0"
        data$Age_col[data$Age_col=="100-110"] <- "100+"
        # data$Age_col <- cut(data$Age,breaks = 10, ordered_result = F)
        data$Date <- as.numeric(data$Date)
        minim_data <- floor(min(data$Date)/10)*10
        ggplot() + 
                geom_line(data = data %>% filter(Type == "main"),
                          aes(x=as.numeric(Date),y=nMx,
                              group=factor(Age),
                              col=factor(Age_col))) +
                geom_point(data = data %>% filter(Type!="main"),
                           aes(x=as.numeric(Date),y=nMx, col=factor(Age_col))) + 
                geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
                geom_vline(xintercept = range(data$Date),linetype=2,col=2)+
                scale_x_continuous(name ="Year",
                                   breaks = seq(minim_data,2020,10), 
                                   labels = seq(minim_data,2020,10))+
                scale_y_log10()+
                ggtitle("Rates by age and time")+
                theme_bw()+
                labs(colour="Age")+
                theme(legend.position="none")+
                facet_grid(~Sex)
}

# plot ex with time variation
plot_ex_time <- function(data){
        data$Date <- as.numeric(data$Date)
        data$Source <- as.factor(data$Source)
        minim_data <- floor(min(data$Date)/10)*10
        ggplot(data = data %>% filter(Age%in%c(0,65)),
               aes(x=as.numeric(Date),y=as.numeric(ex),
                   group=factor(Sex),
                   col=Source)) + 
                geom_line() + geom_point(aes(shape=factor(Sex))) +
                geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
                geom_vline(xintercept = range(data$Date),linetype=2,col=2)+
                scale_x_continuous(name ="Year",
                                   breaks = seq(minim_data,2020,10), 
                                   labels = seq(minim_data,2020,10))+
                labs(y="e(x)",colour = "Source",shape = "Sex")+
                ggtitle("Life expectancy at different ages")+
                theme_bw()+
                theme(legend.position="bottom")+
                facet_wrap(~Age,nrow = 3, scales = "free_y",)
}

# plot dispersion measures
plot_dispersion <- function(data){
        minim_data <- floor(min(data$Date)/10)*10
        ggplot(data %>% 
                pivot_longer(cols = 3:5, names_to = "Indicator")) + 
        geom_line(aes(x=Date, y=value, col=Sex))+
        facet_wrap(~Indicator,nrow = 2,ncol = 2, scales = "free")+
        geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
        geom_vline(xintercept = range(data$Date),linetype=2,col=2)+
        scale_x_continuous(name ="Year",
                           breaks = seq(minim_data,2020,10), 
                           labels = seq(minim_data,2020,10))+
        ggtitle("Dispersion measures")+
        theme_bw()
}

# plot sex ratio measures
plot_sex_ratios <- function(data){
        minim_data <- floor(min(data$Date)/10)*10
        ggplot(data %>% 
                       pivot_longer(cols = 2:4, names_to = "Indicator")) + 
                geom_line(aes(x=Date, y=value, col=Indicator))+
                geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
                geom_vline(xintercept = range(data$Date),linetype=2,col=2)+
                scale_x_continuous(name ="Year",
                                   breaks = seq(minim_data,2020,10), 
                                   labels = seq(minim_data,2020,10))+
                ggtitle("Sex ratio measures (female/male)")+
                theme_bw()
}




# remove zero rates: spline on lx on those zero cells
remove_zero_rates <- function(input, Age_output = 0:100){
        # if(input$Date==1998){browser()}
        if(isTRUE(any(input$nMx==0))){
                # browser()
                input_no_zeros <- input %>% filter(c(1,diff(input$lx))!=0) # remove no risk at all
                lx <- data.frame(Age = Age_output, 
                                 lx = splinefun(x = input_no_zeros$Age, 
                                                y = input_no_zeros$lx, 
                                                method = "monoH.FC")(Age_output))
                out <- lt_single_qx(nqx = lt_id_l_q(lx$lx),
                                    Age = lx$Age, extrapFrom = 100, OAnew = 100)
                out <- bind_cols(out,input[,c("Date","Type","Sex")])
                out$Source <- unique(input$Source) 
                return(out)
        }else{
                return(input)
        }
}

# lee carter fun
lc <- function(input, dates_out, jump_off = TRUE, ...){
     dates_in = sort(unique(as.numeric(input$Date)))
     input %>% split(input$Sex) %>% 
          lapply(function(X){
         # browser()
               M <- X %>% select(Date,nMx,Age) %>% 
                    pivot_wider(names_from = Date, values_from = nMx)   %>% 
                    select(-Age) %>% as.matrix()
               # browser()
               ndates_in <- ncol(M)
               ax  <- rowSums(log(M))/ndates_in
               M_svd      <- svd(log(M)-ax)
               bx         <- M_svd$u[, 1]/sum(M_svd$u[, 1])
               kt         <- M_svd$d[1] * M_svd$v[, 1] * sum(M_svd$u[, 1])  
               # fit kt
               kt_diff <- diff(kt)
               summary_kt <- summary(lm(kt_diff ~ 1))
               kt_drift <- summary_kt$coefficients[1,1]
               h <- dates_out - dates_in[1]
               kt_forecast <- head(kt, 1) + (h * kt_drift)
               # if there is a jump off
               if(!jump_off){
                       if(all(h<0)){
                               ax <- log(M[,1])
                               h <- dates_out - min(dates_in)}
                       if(all(h>0)){
                               ax <- log(M[,ndates_in])
                               h <- dates_out - max(dates_in)}
                       kt_forecast <- h * kt_drift
               }
               M_hat <- exp(ax + bx %*% t(kt_forecast)) %>% as.data.frame()
               colnames(M_hat) <- dates_out
               M_hat$Age <- sort(unique(X$Age))
               M_hat <- M_hat %>%
                    pivot_longer(cols=-ncol(M_hat), names_to="Date",values_to="nMx")
               M_hat$Sex = unique(X$Sex)
               M_hat
               
          }) %>% 
          do.call("rbind", .)
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

