#' Fill data gaps on time serie of life tables for WPP.
#' @description ...
#' @param country   string. Name of the country according locations of WPP.
#' @param dates_out numeric. A decimal vector of dates. By deafault `1950.5:2020.5`.


#' @param LC_fit_e0 logical. Fit k(t) to replicate `e_0`, usign usual LC or the one for limited data (Li, ).
#' @param window_LClim integer. How many years consider to fit LC with limited data.
#' @param window_LC integer. How many years consider to fit LC. If `NULL` then detect following Booth (2002) (not implemented yet). 
#' @param exclude_ds string. For excluding some data source.
#' @param OAnew integer. Useful for `Demotools` functions, to homogenize close out in life tables.
#' @param jump_off logical. Whether or not to start extrapolation from last value (Lee & Miller, 2001). Default `FALSE`.
#' @param prev_cross_over logical. Whether or not to allow crossover between groups. Default `FALSE`, allowing.
#' @details ...
#' @return A list with...
#' @export
#'
#' @examples
#' # Poland case
#' X <- fill_gaps_tier1(country = "Poland")
#' \dontrun{
#' # see diagnsotics plots
#' X$plots$g_country_data
#' X$plots$g_country_rates
#' X$plots$g_country_ex
#' X$plots$g_dispersion
#' X$plots$g_sex_ratios
#' }

fill_gaps_tier1 <- function(country = NULL, dates_out = 1950.5:2020.5,
                           window_LC = 30, window_LClim = 5,
                           exclude_ds = NULL, OAnew = 100,
                           prev_cross_over = FALSE, jump_off = TRUE, LC_fit_e0 = FALSE){

        # country = "Australia"; dates_out = 1950.5:2020.5;
        # window_LC = 30; window_LClim = 5; exclude_ds = NULL; OAnew = 100;
        # # prev_cross_over = FALSE; jump_off = TRUE; LC_fit_e0 = FALSE
        # prev_cross_over = TRUE; jump_off = FALSE; LC_fit_e0 = TRUE
        
        # id and name
        country_full <- get_ID_Name(country)
        cat(paste(country_full))
        
        # get data ----------------------------------------------------------------
        country_raw_data <- 
                read.csv(paste0("data/",country_full$Name,".csv")) %>% 
                select(IndicatorName, DataSourceShortName, DataReliabilitySort,
                       SexName, AgeStart, AgeSpan, TimeLabel, TimeMid, DataValue) %>% 
                filter(SexName!="Both sexes", !DataSourceShortName %in% exclude_ds) %>% 
                mutate(SexName = ifelse(SexName=="Female","f","m")) %>% 
                distinct()
        
        # add WPP19 as base point in 1950-1955
        country_raw_data_wpp19 <- add_wpp19_1950data(country = country_full$PK_LocID,
                                                     country_data = country_raw_data)
                   
        # add avg death time a(x) as other col
        country_raw_data_a <- country_raw_data_wpp19 %>% 
                filter(!str_detect(IndicatorName,"a\\(x")) %>% 
                left_join(country_raw_data %>% 
                                  filter(str_detect(IndicatorName,"a\\(x")) %>% 
                                  select(-1,-3,-8) %>% 
                                  rename(a = DataValue) %>% 
                                  distinct(),
                          by = c("DataSourceShortName","SexName",
                                 "AgeStart","AgeSpan","TimeLabel")) %>%
                group_by_at(vars(-a)) %>% slice(1) %>% 
                ungroup() %>%  distinct()

        # validation
        country_data_validation <- is_data_valid(country_raw_data_a) 
        
        # data with errors
        country_data_errors <- country_data_validation %>% 
                filter(validation=="error")
                # country_data_errors %>% count(DataSourceShortName,TimeLabel,IndicatorName,SexName) %>% tail(20)
        
        # valid data for the country
        country_data <- country_data_validation %>% 
                filter(validation!="error")
        
        # wich data is available
        g_country_data <- plot_data(country_data)
        
        # data selection ----------------------------------------------------------
        
        # lt data
        selected_data <- select_data(country_data)
        
        # data for model life tables
        other_model_data <- country_data %>% 
                filter(!str_detect(IndicatorName,"x")) %>% 
                filter(!TimeMid %in% selected_data$TimeMid)
        
        # detect gaps ----------------------------------------------------------
        
        chunks_data <- split(selected_data$TimeMid, 
                             cumsum(c(1, diff(selected_data$TimeMid) >= 2))) # !=1
        
        main_data_time <- chunks_data[which(lengths(chunks_data)>5)] %>% unlist(.)
        
        years_gaps <- sort(dates_out[which(!floor(dates_out) %in% floor(main_data_time))])
        
        intervals_gaps <- split(years_gaps, cumsum(c(1, diff(years_gaps) != 1)))
        
        # harmonize all lt data to single ages with OAG=100 ------------------------------------------
        
        # if we dont have a huge first gap don´t use wpp19 as a base in 1950
        if(!1950.5 %in% intervals_gaps[[1]] |  
           (1950.5 %in% intervals_gaps[[1]] & !length(intervals_gaps[[1]])>15)){
                selected_data <- selected_data %>% filter(DataSourceShortName!="WPP19")
        }
        
        # type of data - this for what?
        selected_data <- selected_data %>% 
                mutate(Type = ifelse(TimeMid %in% main_data_time, "main", "add"))
        
        # same cut-off at oldest ages
        extrapFrom_single <- min(selected_data$OAG[selected_data$Type=="main"])
        
        # data to graduate
        lt_data_raw <- country_data %>% 
                inner_join(selected_data %>% select(-TimeMid), 
                           by=c("DataSourceShortName","TimeLabel", "IndicatorName")) %>% 
                filter(!is.na(Type), str_detect(IndicatorName,"x")) %>% 
                distinct()
        
        # lts
        lt_data <-  lt_data_raw %>% 
                split(list(.$TimeMid, .$SexName),drop = T) %>% 
                lapply(function(X){
                        # if(any(X$TimeMid==1960.5)){browser()}
                        # browser()
                        print(paste0(unique(X$SexName),"-", unique(X$TimeMid)))
                        Indicator <- unique(X$IndicatorName)
                        Indicator_type = ifelse(str_detect(Indicator,"m\\(x,n"),"mx",
                                         ifelse(str_detect(Indicator,"l\\(x"),"lx",NA))
                        abr_ages <- c(0,1,seq(5,max(X$AgeStart),5))
                        if(!unique(X$complete)){
                                X <- X %>% filter(AgeStart %in% abr_ages)}
                        LT_sex <- unique(X$SexName)
                        # I dont see a way to add nAx as argument - ask TR
                        LT <- lt_ambiguous(nMx_or_nqx_or_lx = X$DataValue,
                                           type = Indicator_type,
                                           Age = X$AgeStart,
                                           Sex = LT_sex,
                                           axmethod = "un",
                                           a0rule = "ak",
                                           extrapFrom = min(extrapFrom_single,max(X$AgeStart)), 
                                           OAnew = 100,
                                           Single = T)
                        LT$Date <- as.numeric(unique(X$TimeMid))
                        LT$Type <- unique(X$Type)
                        LT$Source <- unique(X$DataSourceShortName)
                        LT$Sex  <- LT_sex
                        LT
                }) %>% 
                do.call(rbind,.)
        
        # loop over gaps -----------------------------------
        gap_middle_data <- gap_before_data <- gap_after_data <- lt_data %>% filter(Age==-1)
        
        # remove zeros in case to extrapolate
        lt_data_no_zeros <- lt_data %>% 
                                split(list(.$Date,.$Sex)) %>% 
                                map(remove_zero_rates) %>% # remove zeros proccedure
                                do.call(rbind,.)
        
        # fill gaps
        if(length(intervals_gaps[[1]])!=0){
                # prev_cross_over = TRUE; jump_off = FALSE; LC_fit_e0 = TRUE
                # gap = intervals_gaps[[1]]
                for(gap in intervals_gaps){
                        if(1950.5 %in% gap){
                                if(any(unique(lt_data_no_zeros$Date)<max(gap))){ # Also: here goes model lt data
                                        # pick years for LC lim - TODO: extrapolate from last observed
                                        window <- min(lt_data_no_zeros$Date[lt_data_no_zeros$Type=="main"])+window_LClim
                                        LClim_data <- lt_data_no_zeros %>% 
                                                filter(Date >= 1940, Date <= window)%>% 
                                                arrange(Date,Age) 
                                        # if fit e0
                                        if(!LC_fit_e0){
                                                gap_before_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap),
                                                                                 Single = T, prev_divergence = prev_cross_over)$lt_hat
                                        }else{
                                                # takes last observed and interval not main source points
                                                dates_e0 <- lt_data_no_zeros %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0) %>% 
                                                        select(Date) %>% unique() %>% pull()
                                                e0_Males <- lt_data_no_zeros %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0, Sex == "m") %>% 
                                                        select(ex) %>% unique() %>% pull()
                                                e0_Females <- lt_data_no_zeros %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0, Sex == "f") %>% 
                                                        select(ex) %>% unique() %>% pull()
                                                gap_before_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                                 dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                                 Single = T, prev_divergence = prev_cross_over)$lt_hat
                                        }
                                        gap_before_data <- gap_before_data %>% mutate(Type = "LC-Lim", Source = "LC-Lim")
                                }else{
                                        # apply LC
                                        window <- window_LC
                                        LC_data <- lt_data_no_zeros %>% 
                                                filter(Date < (max(gap)+window))
                                        gap_before_data <- lc(input = LC_data, dates_out = gap, jump_off = jump_off, 
                                                              prev_cross_over = prev_cross_over,
                                                              LC_fit_e0 = LC_fit_e0) %>% 
                                                split(list(.$Date, .$Sex)) %>% 
                                                lapply(function(X){
                                                        LT <- lt_single_mx(nMx = X$nMx,
                                                                           Age = X$Age,
                                                                           Sex = unique(X$Sex),
                                                                           OAnew = 100)
                                                        LT$Sex <- unique(X$Sex)
                                                        LT$Date <- as.numeric(unique(X$Date))
                                                        LT
                                                }) %>% 
                                                do.call(rbind,.) 
                                        gap_before_data <- gap_before_data %>% mutate(Type = "LC", Source = "LC")
                                }
                        }
                        if(2020.5 %in% gap){
                                # pick years for LC lim - TODO: strcutural period selection
                                window <- window_LC
                                LC_data <- lt_data_no_zeros %>% 
                                        filter(Date > (min(gap)-window))
                                # apply LC
                                gap_after_data <- lc(input = LC_data, dates_out = gap, jump_off = jump_off,
                                                     prev_cross_over = prev_cross_over,
                                                     LC_fit_e0 = LC_fit_e0)%>% 
                                        split(list(.$Date, .$Sex), drop = TRUE) %>% 
                                        lapply(function(X){
                                                LT <- lt_single_mx(nMx = X$nMx,
                                                                   Age = X$Age,
                                                                   Sex = unique(X$Sex),
                                                                   OAnew = 100)
                                                LT$Sex <- unique(X$Sex)
                                                LT$Date <- as.numeric(unique(X$Date))
                                                LT
                                        }) %>% 
                                        do.call(rbind,.)
                        }
                        if(!any(c(1950.5,2020.5) %in% gap)){
                                window <- window_LClim
                                LClim_data <- lt_data_no_zeros %>% 
                                        filter(Date >= min(gap)-window, 
                                               Date <= max(gap)+window) %>% 
                                        arrange(Date,Age) 
                                # if fit e0
                                if(!LC_fit_e0){
                                        gap_middle_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap),
                                                                         Single = T, prev_divergence = prev_cross_over)$lt_hat
                                }else{
                                        dates_e0 <- lt_data_no_zeros %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0) %>% 
                                                select(Date) %>% unique() %>% pull()
                                        e0_Males <- lt_data_no_zeros %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0, Sex=="m") %>% 
                                                select(ex) %>% pull()
                                        e0_Females <- lt_data_no_zeros %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0, Sex=="f") %>% 
                                                select(ex) %>% pull()
                                        gap_middle_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                         dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                         Single = T, prev_divergence = prev_cross_over)$lt_hat
                                } 
                        }
                }        
        }
        
        # diagnostics ------------------------------------------------------------
        # final data
        t_final_data <- bind_rows(lt_data %>% filter(Type == "main"),
                                gap_before_data,
                                gap_middle_data %>% mutate(Type = "LC-Lim", Source = "LC-Lim"),
                                gap_after_data %>% mutate(Type = "LC", Source = "LC"))
        
        # dispersion dx
        t_dx_disperison <- t_final_data %>% 
                group_by(Date, Sex) %>% 
                summarise(d_mode = which.max(ndx[Age>5])-1,
                          d_sd = (sum(ndx*Age^2)/lx[Age==0]-(sum(ndx*Age)/lx[Age==0])^2)^.5,
                          d_cv = d_sd/(sum(ndx*Age)/lx[Age==0])*100,
                          e_dagger = -sum(log(lx/lx[Age==0])*lx/lx[Age==0])) %>% 
                select(-d_sd)
        
        # sex ratios
        t_sex_ratio <- t_final_data %>% 
                group_by(Date) %>%
                summarise(`1q0` = nqx[Sex=="f" & Age==0]/nqx[Sex=="m" & Age==0],
                          `35q15` = (1-lx[Sex=="f" & Age==50]/lx[Sex=="f" & Age==15])/
                                            (1-lx[Sex=="m" & Age==50]/lx[Sex=="m" & Age==15]),
                          `35q65` = (1-lx[Sex=="f" & Age==100]/lx[Sex=="f" & Age==65])/
                                            (1-lx[Sex=="m" & Age==100]/lx[Sex=="m" & Age==65]))
        
        # plots
        g_country_ex <- plot_ex_time(bind_rows(t_final_data,lt_data %>% filter(Type == "add",Date<1950.5)), country = country_full$Name)
        g_country_rates <- plot_age_time(t_final_data, country = country_full$Name)
        g_dispersion <- plot_dispersion(t_dx_disperison, country = country_full$Name)
        g_sex_ratios <- plot_sex_ratios(t_sex_ratio, country = country_full$Name)
        g_country_HMD_ex <- g_HMD_ex + 
                        geom_line(data = t_final_data %>% filter(Age %in% c(0,80)),
                                  aes(x=Date,y=ex), color = "red")+
                        ggtitle(paste0("Life expectancy at birth and age 80 for HMD countries and ",country_full$Name))
        load("data/HMD/g_HMD.Rdata")
        g_country_HMD_q <- g_HMD_q + 
                        geom_line(data = t_final_data %>% 
                                          group_by(Date,Sex) %>% 
                                          summarise(`0q5` = 1-lx[Age==5]/lx[Age==0],
                                                    `60q20` = 1-lx[Age==80]/lx[Age==20]),
                                  aes(x=`0q5`,y=`60q20`), color = "red")+
                        ggtitle(paste0("Infant and adult mortality for HMD countries and ",country_full$Name))
        g_country_data <- g_country_data +
                annotate("segment", 
                        x = selected_data$TimeMid[-nrow(selected_data)], 
                        xend = selected_data$TimeMid[-1], 
                        y = selected_data$DataSourceShortName[-nrow(selected_data)], 
                        yend = selected_data$DataSourceShortName[-1],
                        # colour="grey",
                        size=2, alpha=0.5) +
                ggtitle(label = paste0("Source data from ",country_full$Name), 
                        subtitle = "Selected data with a line")

        # return -------------------------------------------------------------------
        out <- list(plots = list(g_country_data = g_country_data, 
                                 g_country_rates = g_country_rates, 
                                 g_country_ex = g_country_ex,
                                 g_country_HMD_ex = g_country_HMD_ex,
                                 g_country_HMD_q = g_country_HMD_q,
                                 g_dispersion = g_dispersion,
                                 g_sex_ratios = g_sex_ratios),
                    tables = list(selected_data = selected_data, 
                                 final_data = t_final_data,
                                 error_data = country_data_errors,
                                 dx_disperison = t_dx_disperison,
                                 sex_ratio = t_sex_ratio,
                                 gaps = intervals_gaps
                                 )
                    )
}

