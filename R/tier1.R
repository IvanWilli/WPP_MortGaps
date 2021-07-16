############## tier 1

##### to do
# same cut off in life tables
# test all tier1
# build function
# set parameters
        # country,
        # method_left_gap = c("LC","other")
        # method_right_gap = c("LCLim","other")
        # LCLim_fit_e0 = FALSE
# output
        # life tables with orgin (source or estimate with which method and data)
        # diagnostic plots
        # comparison with previous WPP
        
# libraries 
xfun::pkg_attach(c("devtools", "data.table","DDSQLtools","DemoTools","jsonlite","openssl","tidyverse"))
source("R/funs.R")

# countrys -----------------------------------------------------------------
locs_tier1 <- read.csv("data/locs_tier1.csv") 

# country code and dates out
locs_tier1_seen <- c("Germany","Belgium","Greece","Denmark",
                     "Japan","Ireland","Luxembourg","Canada",
                     "United States of America","North Macedonia","New Zealand","Czechia")

fillgaps_tier1 <- function(country, LC_fit_e0 = FALSE, window_LC = 30, window_LClim = 2,
                           exclude_ds = NULL){

        # parameters
        country = "Canada"; LC_fit_e0 = FALSE; window_LC = 30; window_LClim = 2; 
        exclude_ds = NULL
        
        # get data ----------------------------------------------------------------
        country_raw_data <- read.csv(paste0("data/",country,".csv")) %>% 
                select(IndicatorName, DataSourceShortName, DataReliabilitySort,
                       SexName, AgeStart, AgeSpan, TimeLabel, TimeMid, TimeDuration, DataValue) %>% 
                filter(SexName!="Both sexes", !DataSourceShortName %in% exclude_ds) %>% 
                mutate(SexName = ifelse(SexName=="Female","f","m")) %>% 
                distinct()
        
        # validation
        country_data_validation <- is_data_valid(country_raw_data) 
        
        # calid data
        country_data <- country_data_validation %>% 
                filter(validation!="error")
        
        # wich data is available
        g_country_data <- plot_data(country_data)
        
        # data selection ----------------------------------------------------------
        
        # always same dates output
        dates_out = 1950.5:2020.5
        
        # lt data
        selected_data <- select_data(country_data)
        
        # data for model tables uses
        other_model_data <- country_data %>% 
                filter(!str_detect(IndicatorName,"x")) %>% 
                filter(TimeMid %in% single_years_gap)
        
        # detect gaps ----------------------------------------------------------
        
        chunks_data <- split(selected_data$TimeMid, cumsum(c(1, diff(selected_data$TimeMid) != 1)))
        
        main_data_time <- chunks_data[[which(lengths(chunks_data)==max(lengths(chunks_data)))]]
        
        single_years_gap <- sort(dates_out[which(!dates_out %in% unique(main_data_time))])
        
        single_intervals_gap <- split(single_years_gap, cumsum(c(1, diff(single_years_gap) != 1)))
        
        if(2020.5 %in% single_intervals_gap[[1]]){
                names(single_intervals_gap) <- "after"
        }else{
                names(single_intervals_gap) <- c("before","after")
        }
        
        # harmonize all lt data to single ages with OAG=100 ------------------------------------------
        
        # type of data
        selected_data <- selected_data %>% 
                mutate(Type = ifelse(TimeMid %in% main_data_time, "main", "add"))
        
        # same cut-off at oldest ages
        extrapFrom_single <- min(selected_data$OAG[selected_data$Type=="main"])
        
        # data to graduate
        lt_data_raw <- country_data %>% 
                inner_join(selected_data, by=c("DataSourceShortName","TimeMid", "IndicatorName")) %>% 
                filter(!is.na(Type), str_detect(IndicatorName,"x")) %>% 
                distinct()
        
        # lt_data_raw %>% count(TimeMid,IndicatorName,SexName)
        
        # lts
        lt_data <-  lt_data_raw %>% 
                split(list(.$TimeMid, .$SexName)) %>% 
                lapply(function(X){
                        # if(any(X$TimeMid==2017)){browser()}
                        # browser()
                        print(paste0(unique(X$SexName),"-", unique(X$TimeMid)))
                        Indicator <- unique(X$IndicatorName)
                        Indicator_type = ifelse(str_detect(Indicator,"m\\(x,n"),"mx",
                                      ifelse(str_detect(Indicator,"l\\(x"),"lx",NA))
                        LT_sex <- unique(X$SexName)
                        LT <- lt_ambiguous(nMx_or_nqx_or_lx = X$DataValue,
                                           type = Indicator_type,
                                           Age = X$AgeStart,
                                           Sex = LT_sex,
                                           # extrapFrom = min(extrapFrom_single,max(X$AgeStart)), 
                                           OAnew = 100,
                                           Single = T)
                        LT$Date <- unique(X$TimeMid)
                        LT$Type <- unique(X$Type)
                        LT$Sex  <- LT_sex
                        LT
                }) %>% 
                do.call(rbind,.)
        
        # plot harmonized sigle lt
        # plot_age_time(lt_data)
        # plot_ex_time(lt_data)
        
        # before -----------------------------------
        gap_before <- single_intervals_gap[["before"]]
        
        # get structure
        gap_before_data <- gap_before_data_fit_e0 <- lt_data %>% filter(Age==-1)
        
        if(!is.null(gap_before)){
                
                # pick years for LC lim - TODO: extrapolate from last observed
                window <- window_LClim
                LClim_data <- lt_data %>% 
                        filter(Date > 1940, Date<(min(lt_data$Date[lt_data$Type=="main"])+window))%>% 
                        mutate(Date = as.numeric(Date)) %>% 
                        arrange(Date,Age) 
                # if fit e0
                if(!LC_fit_e0){
                        gap_before_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap_before),
                                                         Single = T)$lt_hat %>% 
                                mutate(Type = "LC-Lim")
                }else{
                        dates_e0 <- lt_data %>% filter(Type!="main", Age==0) %>% select(Date) %>% unique() %>% pull()
                        e0_Males <- lt_data %>% filter(Type!="main", Age==0, Sex=="m") %>% select(ex) %>% pull()
                        e0_Females <- lt_data %>% filter(Type!="main", Age==0, Sex=="f") %>% select(ex) %>% pull()
                        
                        gap_before_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap_before), 
                                                                dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                Single = T)$lt_hat %>% 
                                mutate(Type = "LC-Lim-fit_e0")
                }
                
        }
        
        # after -------------------------------------------------------------------
        gap_after <- single_intervals_gap[["after"]]
        
        # get structure
        gap_after_data <- lt_data %>% filter(Age==-1)
        
        if(!is.null(gap_after)){
                
                # pick years for LC lim - TODO: strcutural period selection
                window <- window_LC
                LC_data <- lt_data %>% 
                        filter(Date > (min(gap_after)-window)) %>% 
                        split(list(.$Date,.$Sex)) %>% 
                        map(remove_zero_rates) %>% # remove zeros proccedure
                        do.call(rbind,.)
                
                # apply LC
                gap_after_data <- lc(input = LC_data, dates_out = gap_after)%>% 
                        split(list(.$Date, .$Sex)) %>% 
                        lapply(function(X){
                                LT <- lt_single_mx(nMx = X$nMx,
                                                   Age = X$Age,
                                                   Sex = unique(X$Sex),
                                                   OAnew = 100)
                                LT$Sex <- unique(X$Sex)
                                LT$Date <- as.numeric(unique(X$Date))
                                LT$Type <- "LC"
                                LT
                        }) %>% 
                        do.call(rbind,.)
        }
        
        # diagnostics ------------------------------------------------------------
        final_data <- bind_rows(lt_data %>% filter(Type == "main"),
                                gap_before_data,
                                gap_after_data)
        g_country_rates <- plot_age_time(final_data)
        g_country_ex <- plot_ex_time(final_data)
        

        # retun -------------------------------------------------------------------
        out <- list(plots = list(g_country_data = g_country_data, 
                                 g_country_rates = g_country_rates, 
                                 g_country_ex = g_country_ex),
                    selected_data = selected_data, 
                    final_data = final_data)
}

out$plots$g_country_data
out$plots$g_country_rates
out$plots$g_country_ex












if(unique(X$complete)){
        if(X$IndicatorName == "m(x,n) - complete"){
                X <- X %>% filter(IndicatorName == "m(x,n) - complete")
                LT <- lt_single_mx(nMx = X$DataValue,
                                   Age = X$AgeStart,
                                   Sex = LT_sex,
                                   # extrapFrom = min(extrapFrom_single,max(X$AgeStart)), 
                                   OAnew = 100)
        }
        if(X$IndicatorName == "l(x) - complete"){
                X <- X %>% filter(IndicatorName == "l(x) - complete")
                qx <- lt_id_l_q(X$DataValue)
                LT <- lt_single_qx(nqx = qx,
                                   Age = X$AgeStart,
                                   Sex = LT_sex,
                                   # extrapFrom = min(extrapFrom_single,max(X$AgeStart)), 
                                   OAnew = 100)
        }
}else{
        if(X$IndicatorName == "m(x,n) - abridged"){
                X <- X %>% filter(IndicatorName == "m(x,n) - abridged",
                                  AgeStart %in% c(0,1,seq(5,105,5)))
                LT <- lt_abridged2single(nMx = X$DataValue,
                                         Age = X$AgeStart,
                                         Sex = LT_sex,
                                         # extrapFrom = min(extrapFrom_single,max(X$AgeStart)),
                                         OAnew = 100)
        }
        if(X$IndicatorName == "l(x) - abridged"){
                X <- X %>% filter(IndicatorName == "l(x) - abridged",
                                  AgeStart %in% c(0,1,seq(5,105,5)))
                LT <- lt_abridged2single(lx =  X$DataValue,
                                         Age = X$AgeStart,
                                         Sex = LT_sex,
                                         # extrapFrom = min(extrapFrom_single,max(X$AgeStart)),
                                         OAnew = 100)
        }
}














