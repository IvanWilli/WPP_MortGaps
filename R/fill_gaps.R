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
                           prev_cross_over = TRUE, jump_off = FALSE, LC_fit_e0 = TRUE, ...){

        # country = "Greece"; dates_out = 1950.5:2020.5;
        # window_LC = 30; window_LClim = 5; exclude_ds = NULL; OAnew = 100;
        # prev_cross_over = TRUE; jump_off = FALSE; LC_fit_e0 = TRUE
        # ...=NULL
        # 
        # id and name
        country_full <- get_ID_Name(country); cat(paste(country_full))
        
        # get data ----------------------------------------------------------------
        country_db <- read.csv(paste0("data/",country_full$Name,".csv"))
        
        # first manipulation
        country_raw_data <- country_db %>% 
                as.data.table() %>% deduplicates() %>% 
                select(IndicatorName, DataSourceShortName, DataReliabilitySort, DataTypeSort, nrank3,
                       SexName, AgeStart, AgeSpan, TimeLabel, TimeMid, DataValue) %>% 
                filter(SexName!="Both sexes", !DataSourceShortName %in% exclude_ds) %>%
                mutate(SexName = ifelse(SexName=="Female","f","m"),
                       DataSourceShortName = ifelse(DataSourceShortName=="WPP", "VR(WPP)", DataSourceShortName),
                       DataValue = ifelse(DataValue<1,round(DataValue,8),DataValue)) %>% # remove close to zero errors 
                as.data.frame() %>% 
                distinct()
        
        # add WPP19 as base point in 1950-1955
        country_raw_data_wpp19 <- add_wpp19_1950data(country = country_full$PK_LocID, country_data = country_raw_data)

        # get exposures estimates from alst revision as a reference
        pop <- DemoToolsData::popWpp2019x1 %>%
                rename(m=PopMale, f=PopFemale) %>% 
                filter(LocID==country_full$PK_LocID)
                       
        # validation
        country_data_validation <- is_data_valid(country_data = country_raw_data_wpp19, pop = pop) 
        
        # imputation
        country_data_imputed <- impute_data(data = country_data_validation, pop = pop)
        
        # data with errors
        country_data_errors <- country_data_imputed %>% filter(validation %in% c("error","zeros"))

        # valid data for the country
        country_data <- country_data_imputed %>% filter(!validation %in% c("error","zeros"))
        
        # wich data is available
        g_country_data <- plot_data(country_data)
        
        # data selection ----------------------------------------------------------
        
        # lt data selected
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
                inner_join(selected_data %>% select(-TimeMid,-nrank3,-DataTypeSort), 
                           by=c("DataSourceShortName","TimeLabel", "IndicatorName")) %>% 
                filter(!is.na(Type), str_detect(IndicatorName,"x")) %>% 
                distinct()
        
        # lts
        lt_data <-  lt_data_raw %>% 
                split(list(.$TimeMid, .$SexName), drop = T) %>% 
                lapply(function(X){
                        # if(any(X$TimeMid==1953.5)){browser()}
                        # X <- lt_data_raw %>% filter(TimeMid==1995.5,SexName=="f")
                        print(paste0(unique(X$SexName),"-", unique(X$TimeMid)))
                        Indicator <- unique(X$IndicatorName)
                        Indicator_type = ifelse(str_detect(Indicator,"m\\(x,n"),"mx",
                                         ifelse(str_detect(Indicator,"l\\(x"),"lx",NA))
                        abr_ages <- c(0,1,seq(5,max(X$AgeStart),5))
                        # just keep abr ages
                        if(!unique(X$complete)){
                                X <- X %>% filter(AgeStart %in% abr_ages)
                        }
                        # some LT with l(OAG)==0
                        if(Indicator_type=="lx" & last(X$DataValue)==0){
                                X <- X %>% filter(!AgeStart %in% last(abr_ages))
                        }
                        LT_sex <- unique(X$SexName)
                        # I dont see a way to add nAx as argument - ask TR
                        LT <- lt_ambiguous(nMx_or_nqx_or_lx = X$DataValue,
                                           type = Indicator_type,
                                           Age = X$AgeStart,
                                           Sex = LT_sex,
                                           extrapFrom = min(extrapFrom_single,max(X$AgeStart)), 
                                           OAnew = 100,
                                           Single = T,
                                           ...)
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
                # decide for different kind of gaps
                for(gap in intervals_gaps){
                        # gap in the left
                        if(1950.5 %in% gap){
                                # if we have some lonely data point to pivot 
                                if(any(unique(lt_data_no_zeros$Date)<max(gap))){ # Also: here goes model lt data
                                        window <- min(lt_data_no_zeros$Date[lt_data_no_zeros$Type=="main"])+window_LClim
                                        LClim_data <- lt_data_no_zeros %>% 
                                                filter(Date >= 1940, Date <= window)%>% 
                                                arrange(Date,Age) 
                                        # if fit e0 for kt parameter
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
                                        # apply usual LC for extrapolating to 1950
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
                                                                           OAnew = 100,
                                                                           ...)
                                                        LT$Sex <- unique(X$Sex)
                                                        LT$Date <- as.numeric(unique(X$Date))
                                                        LT
                                                }) %>% 
                                                do.call(rbind,.) 
                                        gap_before_data <- gap_before_data %>% mutate(Type = "LC", Source = "LC")
                                }
                        }
                        # gap in the right
                        if(2020.5 %in% gap){
                                # apply usual LC
                                window <- window_LC
                                LC_data <- lt_data_no_zeros %>% filter(Date > (min(gap)-window))
                                gap_after_data <- lc(input = LC_data, dates_out = gap, jump_off = jump_off,
                                                     prev_cross_over = prev_cross_over,
                                                     LC_fit_e0 = LC_fit_e0)%>% 
                                        split(list(.$Date, .$Sex), drop = TRUE) %>% 
                                        lapply(function(X){
                                                LT <- lt_single_mx(nMx = X$nMx,
                                                                   Age = X$Age,
                                                                   Sex = unique(X$Sex),
                                                                   OAnew = 100,
                                                                   ...)
                                                LT$Sex <- unique(X$Sex)
                                                LT$Date <- as.numeric(unique(X$Date))
                                                LT
                                        }) %>% 
                                        do.call(rbind,.)
                        }
                        # central gaps
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
        # final data recovering inputs from LC-Lim
        final_data <- bind_rows(lt_data %>% filter(Type == "main", Date>=1950),
                                gap_before_data,
                                gap_middle_data %>% mutate(Type = "LC-Lim", Source = "LC-Lim"),
                                gap_after_data %>% mutate(Type = "LC", Source = "LC")) %>% 
                        left_join(lt_data %>% filter(Type=="add") %>% 
                                distinct(Date,Source) %>% rename(Source2=Source)) %>% 
                        mutate(Source = ifelse(!is.na(Source2),Source2,Source)) %>% 
                        select(-Source2, -Type)
        # plot_trends(country_full$Name,t_final_data,x="ex")
        
        # return -------------------------------------------------------------------
        out <- list(
                name = country_full,
                country_data = country_data,
                selected_data = selected_data, 
                final_data = final_data,
                error_data = country_data_errors,
                gaps = intervals_gaps)
}

