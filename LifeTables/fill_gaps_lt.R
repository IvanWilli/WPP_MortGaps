# author: IW
# Fill gaps on lt time series of available data, taking care of arguments from InputFiles sheets. 

fill_gaps_lt <- function(country_db = NULL,
                            country_full = NULL, 
                            dates_out = 1950.5:2020.5,
                            OAnew = 100,
                            mort_params = NULL,
                            mort_inputs = NULL,
                            mort_crises = NULL,
                            first_year = 1950,
                            last_year = 2020){

        # parameters from InputFiles
        if(!is.null(mort_params)){
                Empirical_LT_LeeCarter_time_window_fit_LC <- mort_params$Empirical_LT_LeeCarter_time_window_fit_LC  %>% as.integer()
                Empirical_LT_LeeCarter_time_window_fit_Limited_LC <- mort_params$Empirical_LT_LeeCarter_time_window_fit_Limited_LC %>% as.integer()
                Empirical_LT_exclude_data_source <- mort_params$Empirical_LT_exclude_data_source
                Empirical_LT_LeeCarter_non_divergent <- mort_params$Empirical_LT_LeeCarter_non_divergent %>% as.logical()
                Empirical_LT_LeeCarter_jump_off <- mort_params$Empirical_LT_LeeCarter_jump_off  %>% as.logical()
                Empirical_LT_LeeCarter_fit_e0 <- mort_params$Empirical_LT_LeeCarter_fit_e0   %>% as.logical()  
                Empirical_LT_smoothing <- mort_params$Empirical_LT_smoothing %>% as.logical()
                Age_Specific_Mortality_Input_Data <- mort_params$Age_Specific_Mortality_Input_Data
        }

        # receive data ------------------------------------------------------------
        
        # data excluded from InputFiles
        if(!is.na(Empirical_LT_exclude_data_source)){
                Empirical_LT_exclude_data_source <- tolower(trimws(strsplit(Empirical_LT_exclude_data_source,",")[[1]]))
                possible_ds <- c("HMD","EuroStat","EuroStat",    "VR(WPP)","VR(WPP)","VR(WPP)","WHO DB","WHO DB","HLD 2020","HLD 2020","DYB","GBD 2016","GBD 2016","WPP19")
                inputs_excl_ds <- c("hmd","eurostat","eurostats","vr(wpp)","wpp",   "vr (wpp)", "who db","who",   "hld 2020", "hld", "dyb", "gbd 2016","gbd", "wpp19")
                exclude_ds <- possible_ds[inputs_excl_ds %in% Empirical_LT_exclude_data_source]        
        }else{
                exclude_ds <- NULL
        }
        
        # first data wrangling: deduplicate (PG fun), get relevant vars and rename WPP got from VR  
        country_db_dedup <- try(country_db %>% as.data.table() %>% deduplicates())
        if("try-error" %in% class(country_db_dedup)){
                log_print("No VR or Estimated data are in included in downloaded data.")
                stop("No VR or Estimated data are in included in downloaded data.")
        }
        country_raw_data <-  country_db_dedup %>% 
                select(SeriesID, IndicatorID, IndicatorName, DataSourceShortName, DataProcess, 
                       DataReliabilitySort, DataTypeSort, nrank3,
                       SexName, AgeStart, AgeSpan, TimeLabel, TimeMid, DataValue) %>% 
                filter(SexName!="Both sexes", 
                       TimeMid >=1940) %>% 
                mutate(SexName = ifelse(SexName=="Female","f","m"),
                       DataValue = round(DataValue,8), # some countries have irrational low rates, which I must capture as zeroes
                       TimeLabel = as.character(TimeLabel),
                       TimeMid = round(TimeMid,1),
                       TimeMid_floor = trunc(TimeMid),
                       DataSourceShortName = ifelse(DataSourceShortName=="WPP", "VR(WPP)", DataSourceShortName)) %>% 
                filter(!DataSourceShortName %in% exclude_ds) %>% 
                arrange(TimeLabel, TimeMid,
                        SeriesID, IndicatorID, IndicatorName, DataSourceShortName, DataProcess, DataReliabilitySort, DataTypeSort, nrank3,
                        SexName, AgeStart) %>% 
                as.data.frame() %>% 
                distinct()
        
        # !!! TEMPORARY filters until data server is OK
        if(country_full$PK_LocID==152){
                country_raw_data <- country_raw_data %>% filter(TimeMid<2020, 
                                                                DataSourceShortName!="DYB")
        }
        
        # asking VR, Estimates or both ----------------------------------------

        country_raw_data <- bind_rows(
                country_raw_data %>% 
                        filter(DataProcess == "Estimate") %>% 
                        inner_join(mort_inputs %>% select(TimeMid_floor,SexName=Sex,Estimates_include), by = c("SexName", "TimeMid_floor")) %>% 
                        filter(Estimates_include == TRUE),
                country_raw_data %>% 
                        filter(DataProcess == "VR") %>% 
                        left_join(mort_inputs %>% select(TimeMid_floor,SexName=Sex,VR_include), by = c("SexName", "TimeMid_floor")) %>% 
                        filter(VR_include == TRUE),
                country_raw_data %>%
                        filter(!DataProcess %in% c("VR","Estimate") | TimeMid_floor< first_year) # want the rest available
        )

        # validation and zeroes ----------------------------------------------------------
        
        # add WPP19 as base point in 1950-1955 in case it is neccesary
        country_raw_data_wpp19 <- add_wpp19_1950data(country = country_full$PK_LocID, country_data = country_raw_data)

        # get exposures estimates from last revision as a reference (needed at some instance after) 
        pop <- DemoToolsData::popWpp2019x1 %>%
                rename(m=PopMale, f=PopFemale) %>% 
                filter(LocID==country_full$PK_LocID, Time <= last_year)
                       
        # validation: can be labelled ad single or abridged? 
        country_data_validation <- is_data_valid(country_data = country_raw_data_wpp19, pop = pop) 
        
        # smooth zeroes cells in mx or lx
        country_data_imputed <- impute_data(data = country_data_validation, pop = pop)
        
        # data with errors
        country_data_errors <- country_data_imputed %>% filter(validation %in% c("error","zeros"))

        # valid data for the country
        country_data <- country_data_imputed %>% filter(!validation %in% c("error","zeros"))
        
        # data selection ----------------------------------------------------------
        
        # data selection following criterias
        selected_data <- select_data(country_data)
        
        # detect main time serie and gaps ----------------------------------------------------------
        
        chunks_data <- split(selected_data$TimeMid[selected_data!="WPP19"], 
                             cumsum(c(1, diff(selected_data$TimeMid[selected_data!="WPP19"]) >= 2))) # !=1
        
        main_data_time <- chunks_data[which(lengths(chunks_data)>5)] %>% unlist(.)
        
        # define intervals of gap: take care of some countries with sparse data points and any consecutive data
        if(!is.null(main_data_time)){
                years_gaps <- sort(dates_out[which(!floor(dates_out) %in% floor(main_data_time))])
                intervals_gaps <- split(years_gaps, cumsum(c(1, diff(years_gaps) != 1)))
        }else{
                years_gaps <- sort(dates_out[which(!floor(dates_out) %in% floor(selected_data$TimeMid[selected_data!="WPP19"]))])
                intervals_gaps <- split(years_gaps, cumsum(c(1, diff(years_gaps) != 1)))
        }
        
        # if we dont have a huge first gap don´t use wpp19 as a base in 1950
        if(!(first_year+.5) %in% intervals_gaps[[1]] |  
           ((first_year+.5) %in% intervals_gaps[[1]] & !length(intervals_gaps[[1]])>15)){
                selected_data <- selected_data %>% filter(DataSourceShortName!="WPP19")
        }
        
        # is part of the main time serie or is an additional data point, as a resoruce for pivoting in the filling
        selected_data <- selected_data %>% 
                mutate(Type = ifelse(TimeMid %in% main_data_time, "main", "add"))
        
        # same cut-off at oldest ages
        if(any(selected_data$Type=="main")){
                extrapFrom_single <- min(selected_data$OAG[selected_data$Type=="main"])
        }else{
                extrapFrom_single <- max(selected_data$OAG)
        }
        
        # keep life table data that was selected
        lt_data_raw <- country_data %>% 
                inner_join(selected_data %>% select(-nrank3,-DataTypeSort), 
                           by=c("DataSourceShortName","TimeLabel", "IndicatorName","TimeMid")) %>% 
                filter(!is.na(Type), str_detect(IndicatorName,"x")) %>% 
                mutate(TimeMid_floor = floor(TimeMid)) %>% 
                distinct()
        
        # harmonize all lt data to single ages ans same close-out ------------------------------------------
        lt_data <-  lt_data_raw %>% 
                split(list(.$TimeMid, .$SexName), drop = T) %>% 
                lapply(function(X){
                        print(paste0(unique(X$SexName),"-", unique(X$TimeMid)))
                        # if(unique(X$TimeMid==1961.5) & unique(X$SexName=="f")){browser()}
                        # X <- lt_data_raw %>% filter(TimeMid==1966.5,SexName=="f")
                        X_TimeMid <- unique(X$TimeMid)
                        X_TimeMid_floor <- unique(X$TimeMid_floor)
                        X_sex <- unique(X$SexName)
                        X_DataProcess <- unique(X$DataProcess)
                        X_ages <- X$AgeStart
                        Indicator_type = ifelse(str_detect(unique(X$IndicatorName),"m\\(x,n"),"mx",
                                        ifelse(str_detect(unique(X$IndicatorName),"l\\(x"),"lx",NA))
                        abr_ages <- c(0,1,seq(5,max(X$AgeStart),5))
                        # just keep abr ages in case
                        if(Indicator_type=="lx" & !unique(X$complete)){
                                X <- X %>% filter(AgeStart %in% abr_ages)
                        }
                        # avoid some LT with l(OAG)==0
                        if(Indicator_type=="lx" & last(X$DataValue)==0){
                                X <- X %>% filter(!AgeStart %in% last(abr_ages))
                        }
                        # avoid super rates that crush DemoTools extrap
                        if(Indicator_type=="mx"){
                                Age_m_bigger_1 <- min(X$AgeStart[X$DataValue>1])
                                X <- X %>% filter(AgeStart<Age_m_bigger_1)
                        }
                        
                        # see inputs for adjustments
                        X_inputs <- mort_inputs %>% filter(TimeMid_floor == X_TimeMid_floor, Sex == X_sex)
                        
                        # adjust infant-child if was defined in InputFiles
                        adjust_LT_infant <- ifelse(nrow(X_inputs)==0, F,
                                                   X_inputs$adjust_LT_under_five)
                        if(adjust_LT_infant){ # only for Abr selection
                                if(Age_Specific_Mortality_Input_Data == "Abridged" & !unique(X$complete)){
                                        q0_1     <- X_inputs$q1_input
                                        q0_5     <- X_inputs$q5_input
                                        q1_4     <- 1-(1-q0_5)/(1-q0_1)
                                        X_radix  <- X$DataValue[X$AgeStart==0]
                                        new_lx   <- X_radix * c(1, 1-q0_1, 1-q0_5)
                                        new_a0_1 <- DemoTools::lt_rule_1a0_ak(q0 = q0_1, Sex = X_sex)
                                        new_m0_1 <- q0_1/(1-(1-new_a0_1)*q0_1)
                                        new_a1_4 <- DemoTools::lt_rule_4a1_cd(M0 = new_m0_1, Sex = X_sex, region = "w")
                                        new_m1_4 <- q1_4/(4-(4-new_a1_4)*q1_4)
                                        if(Indicator_type=="lx"){
                                                X$DataValue[X$AgeStart==1] <- new_lx[2]
                                                X$DataValue[X$AgeStart==5] <- new_lx[3]
                                                # just for consistency, take car of increasing lx
                                                X$DataValue[X$AgeStart>5] <- pmin(new_lx[3], X$DataValue[X$AgeStart>5])
                                                X_no_zeros <- X %>% filter(c(-1,diff(X$DataValue))<0) # remove no risk at all
                                                Age_output <- X$AgeStart
                                                lx_no_zeros <- data.frame(Age = Age_output, 
                                                                 lx = splinefun(x = X_no_zeros$AgeStart, 
                                                                                y = X_no_zeros$DataValue, 
                                                                                method = "monoH.FC")(Age_output))
                                                X$DataValue <- lx_no_zeros$lx
                                        }
                                        if(Indicator_type=="mx"){
                                                X$DataValue[X$AgeStart==0] <- new_m0_1
                                                X$DataValue[X$AgeStart==1] <- new_m1_4
                                        }
                                }        
                        }
                        
                        # Old-age adjustments if was defined in InputFiles
                        adjust_LT_oldage <- ifelse(nrow(X_inputs)==0, F, X_inputs$adjust_LT_oldage)
                        # six groups as minimum data
                        min_age_fit <- sort(X$AgeStart, T)[6]
                        N_fit <- ifelse(unique(X$complete),1,5)
                        if(adjust_LT_oldage){
                                X_extrapLaw <- X_inputs$extrapLaw
                                X_extrapFrom <- min(X_inputs$extrapFrom, max(X$AgeStart))
                                X_range_age_fit <- as.numeric(stringr::str_extract_all(X_inputs$extrapFit, "\\d+")[[1]])
                                X_extrapFit <- seq(min(min_age_fit, X_range_age_fit[1]),
                                                   min(max(X$AgeStart), ifelse(is.na(X_range_age_fit[2]),1e3,X_range_age_fit[2])),
                                                       N_fit)
                                # coKannisto needs both sex
                                if(X_extrapLaw == "coKannisto"){
                                        # the other sex
                                        Y <- lt_data_raw %>% filter(TimeMid == X_TimeMid,
                                                                    DataProcess == X_DataProcess,
                                                                    SexName != X_sex)
                                        # conver first to rates in case it´s lx, not extrapolating
                                        if(Indicator_type=="lx" & !unique(Y$complete)){
                                                Y <- Y %>% filter(AgeStart %in% abr_ages)
                                        }
                                        if(Indicator_type=="lx" & last(Y$DataValue)==0){
                                                Y <- Y %>% filter(!AgeStart %in% last(abr_ages))
                                        }
                                        # avoid super rates that crush DemoTools extrap
                                        if(Indicator_type=="mx"){
                                                X <- X %>% filter(DataValue<=1)
                                        }
                                        X_LT <- lt_ambiguous(nMx_or_nqx_or_lx = X$DataValue,
                                                             type = Indicator_type,
                                                             Age = X$AgeStart,
                                                             Sex = X_sex,
                                                             Single = T)
                                        Y_LT <- lt_ambiguous(nMx_or_nqx_or_lx = Y$DataValue,
                                                             type = Indicator_type,
                                                             Age = Y$AgeStart,
                                                             Sex = unique(Y$SexName),
                                                             Single = T)
                                        if(X_sex == "m"){
                                                mxM = X_LT %>% select(nMx)
                                                mxF = Y_LT %>% select(nMx)
                                        }else{
                                                mxM = Y_LT %>% select(nMx)
                                                mxF = X_LT %>% select(nMx)}
                                        rownames(mxM) <- rownames(mxM) <- X_LT$Age
                                        XY_LT <- cokannisto(mxM, mxF,
                                                            est.ages = X_extrapFit, 
                                                            proj.ages = X_extrapFrom:OAnew)
                                        if(X_sex == "m"){
                                                X_LT <- as.numeric(XY_LT$male)
                                        }else{
                                                X_LT <- as.numeric(XY_LT$female)
                                        }
                                        LT <- lt_single_mx(X_LT,Age = 0:OAnew)
                                }else{
                                        LT <- lt_ambiguous(nMx_or_nqx_or_lx = X$DataValue,
                                                           type = Indicator_type,
                                                           Age = X$AgeStart,
                                                           Sex = X_sex,
                                                           extrapLaw = X_extrapLaw,
                                                           extrapFrom = X_extrapFrom,
                                                           extrapFit = X_extrapFit,
                                                           OAnew = OAnew,
                                                           Single = T)      
                                }
                        }else{
                                X_extrapLaw <- NULL
                                X_extrapFrom <- min(extrapFrom_single,max(X$AgeStart))
                                X_extrapFit <- seq(min(min_age_fit,60),max(X$AgeStart),N_fit)
                                LT <- lt_ambiguous(nMx_or_nqx_or_lx = X$DataValue,
                                                   type = Indicator_type,
                                                   Age = X$AgeStart,
                                                   Sex = X_sex,
                                                   extrapLaw = X_extrapLaw,
                                                   extrapFrom = X_extrapFrom,
                                                   extrapFit = X_extrapFit,
                                                   OAnew = OAnew,
                                                   Single = T)
                        }

                        # VR coverage adjustment if was defined in InputFiles
                        adjust_VR_completeness <- ifelse(is.na(X_DataProcess) | X_DataProcess != "VR" | nrow(X_inputs)==0, 1, 
                                                         ifelse(X_inputs$VR_completeness>.6, 1, X_inputs$VR_completeness))
                        LT_VR <- DemoTools::lt_single_mx(nMx = LT$nMx/adjust_VR_completeness,
                                              Age = LT$Age,
                                              Sex = LT$Sex,
                                              extrapFrom = OAnew)
                        
                        # fnial vars
                        LT$Date <- as.numeric(unique(X$TimeMid))
                        LT$Type <- unique(X$Type)
                        LT$Source <- unique(X$DataSourceShortName)
                        LT$Sex  <- X_sex
                        LT$DataProcess <- X_DataProcess
                        
                        # if last age is NA (some DemoTools issue with super rates) set to 50% more than previous
                        LT$nMx[LT$Age==100 & is.na(LT$nMx)] <- LT$nMx[LT$Age==99] * 1.5
                        
                        return(LT)
                }) %>% 
                bind_rows()
        
        # loop over gaps and estimate a LC model -----------------------------------
        
        # take structure from lt_data
        gap_middle_data <- gap_before_data <- gap_after_data <- lt_data %>% filter(Age==-1)
        
        # fill depending the nature of the gap
        if(length(intervals_gaps[[1]])!=0){
                # decide for different kind of gaps
                for(gap in intervals_gaps){
                        print(gap)
                        # gap in the left
                        if((first_year+.5) %in% gap){
                                # if we have some lonely data point to pivot 
                                if(any(unique(lt_data$Date)<max(gap))){
                                        window <- min(lt_data$Date[lt_data$Type=="main"])+Empirical_LT_LeeCarter_time_window_fit_Limited_LC
                                        LClim_data <- lt_data %>% 
                                                filter(Date >= 1940, Date <= window) %>% 
                                                arrange(Date,Age) 
                                        # if fit e0 for kt parameter
                                        if(!Empirical_LT_LeeCarter_fit_e0){
                                                gap_before_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap),
                                                                                 Single = T, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat
                                        }else{
                                                # takes last observed and interval not main source points
                                                dates_e0 <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0) %>% 
                                                        select(Date) %>% unique() %>% pull()
                                                e0_Males <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0, Sex == "m") %>% 
                                                        select(ex) %>% unique() %>% pull()
                                                e0_Females <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0, Sex == "f") %>% 
                                                        select(ex) %>% unique() %>% pull()
                                                gap_before_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                                 dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                                 Single = T, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat
                                        }
                                        gap_before_data <- gap_before_data %>% mutate(Type = "LC-Lim", Source = "LC-Lim")
                                }else{
                                        # apply usual LC for extrapolating to 1950
                                        window <- Empirical_LT_LeeCarter_time_window_fit_LC
                                        LC_data <- lt_data %>% 
                                                filter(Date <= (max(gap)+window))
                                        gap_before_data <- lc(input = LC_data, dates_out = gap, 
                                                              jump_off = Empirical_LT_LeeCarter_jump_off, 
                                                              prev_cross_over = Empirical_LT_LeeCarter_non_divergent,
                                                              LC_fit_e0 = Empirical_LT_LeeCarter_fit_e0) %>% 
                                                        split(list(.$Date, .$Sex)) %>% 
                                                        lapply(function(X){
                                                                LT <- lt_single_mx(nMx = X$nMx,
                                                                                   Age = X$Age,
                                                                                   Sex = unique(X$Sex))
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
                                window <- Empirical_LT_LeeCarter_time_window_fit_LC
                                LC_data <- lt_data %>% filter(Date >= (min(gap)-window))
                                if(length(unique(LC_data$Date))==2){
                                        LClim_data <- LC_data
                                        other_dates <- unique(lt_data$Date[!lt_data$Date %in% unique(LClim_data$Date)])
                                        dist_to_include <- abs(other_dates-mean(unique(LClim_data$Date)))
                                        other_date_to_include <- other_dates[dist_to_include==min(dist_to_include)][1]
                                        LClim_data <- bind_rows(LClim_data,
                                                                lt_data %>% filter(Date == other_date_to_include))%>% 
                                                arrange(Date,Age)
                                        if(!Empirical_LT_LeeCarter_fit_e0){
                                                gap_middle_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap),
                                                                                 Single = T, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat
                                        }else{
                                                dates_e0 <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0) %>% 
                                                        select(Date) %>% unique() %>% pull()
                                                e0_Males <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0, Sex=="m") %>% 
                                                        select(ex) %>% pull()
                                                e0_Females <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0, Sex=="f") %>% 
                                                        select(ex) %>% pull()
                                                gap_after_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                                   dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                                   Single = T, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat %>% 
                                                        mutate(Type = "LC-Lim", Source = "LC-Lim")
                                        }
                                }
                                if(length(unique(LC_data$Date)) %in% 3:5){
                                        LClim_data <- LC_data
                                        if(!Empirical_LT_LeeCarter_fit_e0){
                                                gap_before_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap),
                                                                                 Single = T, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat
                                        }else{
                                                # takes last observed and interval not main source points
                                                dates_e0 <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0) %>% 
                                                        select(Date) %>% unique() %>% pull()
                                                e0_Males <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0, Sex == "m") %>% 
                                                        select(ex) %>% unique() %>% pull()
                                                e0_Females <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0, Sex == "f") %>% 
                                                        select(ex) %>% unique() %>% pull()
                                                gap_after_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                                 dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                                 Single = T, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat %>% 
                                                        mutate(Type = "LC-Lim", Source = "LC-Lim")
                                        }
                                }
                                if(length(unique(LC_data$Date))>5){
                                        gap_after_data <- lc(input = LC_data, dates_out = gap, 
                                                             jump_off = Empirical_LT_LeeCarter_jump_off,
                                                             prev_cross_over = Empirical_LT_LeeCarter_non_divergent,
                                                             LC_fit_e0 = Empirical_LT_LeeCarter_fit_e0) %>% 
                                                split(list(.$Date, .$Sex), drop = TRUE) %>% 
                                                lapply(function(X){
                                                        LT <- lt_single_mx(nMx = X$nMx,
                                                                           Age = X$Age,
                                                                           Sex = unique(X$Sex))
                                                        LT$Sex <- unique(X$Sex)
                                                        LT$Date <- as.numeric(unique(X$Date))
                                                        LT
                                                }) %>% 
                                                do.call(rbind,.)  %>% mutate(Type = "LC", Source = "LC") 
                                }
                        }
                        # central gaps
                        if(!any((c(first_year,last_year)+.5) %in% gap)){
                                window <- Empirical_LT_LeeCarter_time_window_fit_Limited_LC
                                LClim_data <- lt_data %>% 
                                        filter(Date >= min(gap)-window, 
                                               Date <= max(gap)+window) %>% 
                                        arrange(Date,Age) 
                                # sparse data can only take two point. LCLim needs three
                                if(length(unique(LClim_data$Date))==2){
                                        other_dates <- unique(lt_data$Date[!lt_data$Date%in%unique(LClim_data$Date)])
                                        dist_to_include <- abs(other_dates-mean(unique(LClim_data$Date)))
                                        other_date_to_include <- other_dates[dist_to_include==min(dist_to_include)][1]
                                        LClim_data <- bind_rows(LClim_data,
                                                                lt_data %>% filter(Date == other_date_to_include))%>% 
                                                arrange(Date,Age) 
                                }
                                # if fit e0
                                if(!Empirical_LT_LeeCarter_fit_e0){
                                        gap_middle_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap),
                                                                         Single = T, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat
                                }else{
                                        dates_e0 <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0) %>% 
                                                select(Date) %>% unique() %>% pull()
                                        e0_Males <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0, Sex=="m") %>% 
                                                select(ex) %>% pull()
                                        e0_Females <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0, Sex=="f") %>% 
                                                select(ex) %>% pull()
                                        gap_middle_data_i <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                         dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                         Single = T, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat
                                } 
                                gap_middle_data <- bind_rows(gap_middle_data,gap_middle_data_i)
                        }
                }        
        }
        
        # bind results ------------------------------------------------------------
        
        # final data recovering inputs from LC-Lim
        final_data <- bind_rows(lt_data %>% filter(Type == ifelse(is.null(main_data_time),"add","main"), Date>=first_year),
                                gap_before_data,
                                gap_middle_data %>% mutate(Type = "LC-Lim", Source = "LC-Lim"),
                                gap_after_data) %>% 
                        left_join(lt_data %>% filter(Type=="add") %>% 
                                distinct(Date,Source) %>% rename(Source2=Source),
                                by = "Date") %>%
                        mutate(Source = ifelse(!is.na(Source2),Source2,Source)) %>% 
                        select(-Source2, -Type) %>% 
                        filter(Source!="WPP19")
        # plot_ex_time(final_data,country = "pepe")
        
        # empirical smoothing -----------------------------------------------------

        final_data_smooth <- final_data %>% 
                split(.$Sex) %>% 
                lapply(function(X){
                        # X <- final_data %>% filter(Sex=="f")
                        X_sex <- unique(X$Sex)
                        X_Ages <- unique(X$Age)
                        X_Years <- first_year:last_year
                        # rates
                        M <- X %>%
                                filter(Sex==X_sex) %>% 
                                select(Date,nMx,Age) %>%
                                arrange(Date,Age) %>% 
                                pivot_wider(names_from = Date, values_from = nMx)   %>% 
                                select(-Age) %>% as.matrix()
                        # exposed from wpp19
                        E <- pop %>%
                                select(Age=AgeGrpStart, Date=MidPeriod, E=m) %>% 
                                mutate(E = E * 1000) %>% 
                                arrange(Date,Age) %>% 
                                pivot_wider(names_from = Date, values_from = E)   %>% 
                                select(-Age) %>% as.matrix()
                        # add 1 exposure in case 0
                        E[E==0] <- 2 
                        # implicit deaths
                        if(!all(dim(E)==dim(M))){
                                log_print(paste0("The sex ",X_sex," has different number of life tables than the other sex. Some data issue must be chekced."))
                                stop(paste0("The sex ",X_sex," has different life tables than the other sex. Some data issue must be chekced."))
                        }
                        D <- M * E
                        # model options
                        # StMoMo - APC model
                        APC <- StMoMo::apc()
                        APCfit <- StMoMo::fit(APC, Dxt =D, Ext = E, ages = X_Ages, 
                                              years=X_Years, verbose = FALSE)
                        M_smooth_APC <- fitted(APCfit, type = "rates") 
                        M_hat_APC <- M_smooth_APC %>% 
                                        as.data.frame() %>% 
                                        mutate(Age = X_Ages) %>% 
                                        pivot_longer(cols=-Age, names_to="Date",values_to="nMx") %>% 
                                        mutate(Sex = X_sex)
                        LT_hat_APC <- M_hat_APC %>% split(.$Date) %>% 
                                        lapply(function(Y){
                                                # Y = M_hat %>% filter(Date == 1950)
                                                LT_hat <- lt_single_mx(nMx = Y$nMx, Age = Y$Age, 
                                                                       Sex = unique(Y$Sex), extrapFrom = OAnew)
                                                LT_hat$Sex <- unique(Y$Sex)
                                                LT_hat$Date <- as.numeric(unique(Y$Date)) + .5
                                                LT_hat
                                        }) %>% 
                                bind_rows() %>% 
                                mutate(Model = "APC")
                        # MortalitySmooth - AP model
                        # sometimes error in optimization. return un-smoothed matrix and give log message
                        fit2D <- try(MortalitySmooth::Mort2Dsmooth(x=X_Ages, y=X_Years, Z=D, 
                                                                   offset=log(E), method = 1))
                        M_smooth_MS <- try(exp(fit2D$logmortality))
                        if("try-error" %in% class(fit2D)){
                                fit2D <- log(cbind(M[,1],t(apply(M,1,zoo::rollmean,k=3)),M[,ncol(M)]))
                                M_smooth_MS <- exp(fit2D)
                                colnames(M_smooth_MS) <- as.character(dates_out)
                                log_print("MortalitySmooth gave an error. It was reaplaced by a 3-year moving average on time")
                        }
                        M_hat_MS <- M_smooth_MS %>% 
                                as.data.frame() %>% 
                                mutate(Age = X_Ages) %>% 
                                pivot_longer(cols=-Age, names_to="Date",values_to="nMx") %>% 
                                mutate(Sex = X_sex)
                        LT_hat_MS <- M_hat_MS %>% split(.$Date) %>% 
                                lapply(function(Y){
                                        LT_hat <- lt_single_mx(nMx = Y$nMx, Age = Y$Age, 
                                                               Sex = unique(Y$Sex), extrapFrom = OAnew)
                                        LT_hat$Sex <- unique(Y$Sex)
                                        LT_hat$Date <- as.numeric(unique(Y$Date)) + .5
                                        LT_hat
                                }) %>% 
                                bind_rows() %>% 
                                mutate(Model = "AP")
                        LT_hat <- bind_rows(LT_hat_APC, LT_hat_MS)
                        return(LT_hat)
                }) %>% 
                bind_rows() %>% 
                left_join(final_data %>% select(Age, Date, Sex, Source, DataProcess), by = c("Age", "Sex", "Date"))
        
        # return what was selected in InputFiles
        if(Empirical_LT_smoothing == "APC" | isTRUE(Empirical_LT_smoothing)){     
                final_LT <- final_data_smooth %>% filter(Model == "APC") %>% select(-Model)
        }else if(Empirical_LT_smoothing == "AP"){
                final_LT <- final_data_smooth %>% filter(Model == "AP") %>% select(-Model)
        }else{
                final_LT <- final_data
        }


        # add mort crisis ---------------------------------------------------------
        final_data_mc <- final_LT %>% 
                left_join(mort_crises %>% select(Date=TimeMid, Age=age_start, Sex=Sex, value),
                          by = c("Age", "Date", "Sex")) %>% 
                split(list(.$Date, .$Sex), drop = T) %>% 
                lapply(function(X){
                        X$value <-  ifelse(is.na(X$value), 0, X$value)
                        X$nMx_adj <- X$nMx + X$value
                        LT <- lt_single_mx(X$nMx_adj, X$Age, Sex = unique(X$Sex), extrapFrom = OAnew)
                        LT$Date <- as.numeric(unique(X$Date))
                        LT$Source <- unique(X$Source)
                        LT$Sex  <- unique(X$Sex)
                        return(LT)
                }) %>% bind_rows()
        
        # outputs -----------------------------------------------------------------
        
        # recover IDseries
        selected_data_series <- selected_data %>% 
                left_join(country_data %>% 
                        distinct(TimeLabel, IndicatorID, IndicatorName, SeriesID, DataSourceShortName),
                          by = c("DataSourceShortName", "TimeLabel", "IndicatorName"))
        
        # tables for writing in InputFiles fromat
        life_table_age_sex <- 
                final_data_mc %>% 
                pivot_longer(cols = nMx:ex,names_to = "indicator", values_to = "value") %>% 
                mutate(indicator = paste0("lt_",indicator),
                       sex = ifelse(Sex == "f", "female", "male"),
                       time_start = trunc(Date),
                       time_span = 1,
                       age_span = ifelse(Age == OAnew,1000,1)) %>% 
                select(indicator,time_start,time_span,sex,age_start=Age,age_span,value) %>% 
                arrange(indicator, time_start, sex, age_start)
        
        dd_selected_series <- selected_data_series %>% 
                mutate(Status = "Used", SeriesKey = as.character(SeriesID)) %>% 
                select(IndicatorID, SeriesKey, Status)
        
        # abridged output
        final_data_mc_abr <- final_data_mc %>% 
                split(list(.$Date, .$Sex), drop = T) %>% 
                lapply(function(X){
                        LT <- lt_single2abridged(nMx = X$nMx,
                                                     lx = X$lx,
                                                     nLx = X$nLx,
                                                     ex = X$ex, 
                                                     Age = X$Age, 
                                                     Sex = unique(Y$Sex))
                        LT$Sex <- unique(X$Sex)
                        LT$Date <- as.numeric(unique(X$Date))
                        LT$Source <- unique(X$Source)
                        LT$Sex  <- unique(X$Sex)
                        LT
                }) %>% bind_rows()

        # final output
        out <- list(
                name = country_full,
                input_data = country_data,
                selected_data = selected_data_series, 
                output_data = final_data_mc,
                output_data_abr = final_data_mc_abr,
                data_pre_smooth = final_data, 
                data_with_smooth = final_data_smooth, 
                error_data = country_data_errors,
                gaps = intervals_gaps,
                life_table_age_sex = life_table_age_sex,
                dd_selected_series = dd_selected_series)
        return(out)
}

