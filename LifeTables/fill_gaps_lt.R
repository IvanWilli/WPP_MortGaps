# author: IW
# Fill gaps on lt time series of available data, taking care of arguments from InputFiles sheets. 

fill_gaps_lt <- function(country_db = NULL,
                            country_full = NULL, 
                            dates_out = 1950.5:2020.5,
                            OAnew = 100,
                            mort_params = NULL,
                            mort_inputs = NULL,
                            mort_crises = NULL,
                            mlts = NULL,
                            first_year = 1950,
                            last_year = 2020){

        # parameters from InputFiles
        if(!is.null(mort_params)){
                Empirical_LT_LeeCarter_time_window_fit_LC <- mort_params$Empirical_LT_LeeCarter_time_window_fit_LC  %>% as.integer()
                Empirical_LT_LeeCarter_time_window_fit_Limited_LC <- mort_params$Empirical_LT_LeeCarter_time_window_fit_Limited_LC %>% as.integer()
                Empirical_LT_exclude_data_source <- tolower(mort_params$Empirical_LT_exclude_data_source)
                Empirical_LT_LeeCarter_non_divergent <- mort_params$Empirical_LT_LeeCarter_non_divergent %>% as.logical()
                Empirical_LT_LeeCarter_jump_off <- mort_params$Empirical_LT_LeeCarter_jump_off  %>% as.logical()
                Empirical_LT_LeeCarter_fit_e0 <- mort_params$Empirical_LT_LeeCarter_fit_e0   %>% as.logical()  
                Empirical_LT_smoothing_output <- tolower(mort_params$Empirical_LT_smoothing_output %>% as.character())
                Empirical_LT_smoothing_input <- tolower(mort_params$Empirical_LT_smoothing_input %>% as.character())
                Age_Specific_Mortality_Input_Data <- tolower(mort_params$Age_Specific_Mortality_Input_Data %>% as.character())
        }

        # handle strings ----------------------------------------------------------

        # data excluded from InputFiles
        if(!is.na(Empirical_LT_exclude_data_source)){
                Empirical_LT_exclude_data_source <- trimws(strsplit(Empirical_LT_exclude_data_source,",")[[1]])
                possible_ds <- c("HMD","EuroStat","EuroStat",    "VR(WPP)","VR(WPP)","VR(WPP)","WHO DB","WHO DB","HLD 2020","HLD 2020","DYB","GBD 2016","GBD 2016","WPP19")
                inputs_excl_ds <- c("hmd","eurostat","eurostats","vr(wpp)","wpp",   "vr (wpp)", "who db","who",   "hld 2020", "hld", "dyb", "gbd 2016","gbd", "wpp19")
                exclude_ds <- possible_ds[inputs_excl_ds %in% Empirical_LT_exclude_data_source]        
        }else{
                exclude_ds <- NULL
        }
        # input data
        possible_input <- c("abridged","abr",     "complete","single")
        ok_input       <- c("abridged","abridged","complete","complete")
        Age_Specific_Mortality_Input_Data <- ok_input[possible_input %in% Age_Specific_Mortality_Input_Data]
        
        # receive data ------------------------------------------------------------
        
        # deduplicate (PG fun)  
        country_db_dedup <- try(country_db %>% as.data.table() %>% deduplicates())
        if("try-error" %in% class(country_db_dedup) | all(unique(country_db_dedup$TimeMid)<1950)){
                log_print("No VR or Estimated data are in included in downloaded data for the estimation period.")
                stop("No VR or Estimated data are in included in downloaded data for the estimation period.")
        }
        
        # get relevant vars and rename WPP got from VR
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
                       DataSourceShortName = ifelse(DataSourceShortName=="WPP", "VR(WPP)", DataSourceShortName),
                       DataTypeSort = as.integer(DataTypeSort)) %>% 
                filter(!DataSourceShortName %in% exclude_ds) %>% 
                arrange(TimeLabel, TimeMid,
                        SeriesID, IndicatorID, IndicatorName, DataSourceShortName, DataProcess, DataReliabilitySort, DataTypeSort, nrank3,
                        SexName, AgeStart) %>% 
                as.data.frame() %>% 
                distinct()
        
        # # !!! TEMPORARY filters until data server is OK
        # if(country_full$PK_LocID==152){
        #         country_raw_data <- country_raw_data %>% filter(TimeMid<2020, 
        #                                                         DataSourceShortName!="DYB")
        # }
        
        # what year-sex-type of data wnats in ----------------------------------------
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
        if(nrow(country_raw_data)==0){
                stop("You filtered out all the (tiny) available data")
        }
        
        # add WPP19 as a base point in 1950-1955 in case it is neccesary
        country_raw_data_wpp19 <- add_wpp19_1950data(country = country_full$PK_LocID, country_data = country_raw_data)
        
        # get exposures estimates from last revision as a reference (needed at some instance after) 
        pop <- DemoToolsData::WPP2019_pop %>%
                rename(m=PopMale, f=PopFemale) %>% 
                filter(LocID==country_full$PK_LocID, Year <= last_year)

        # validation and zeroes ----------------------------------------------------------

        # validation: can be labelled ad single or abridged? 
        country_data_validation <- is_data_valid(country_data = country_raw_data_wpp19, pop = pop) 
        
        # smooth zeroes cells in mx or lx, group 1:4, keep worth ages
        country_data_imputed <- impute_data(data = country_data_validation, pop = pop)
        
        # data with errors
        country_data_errors <- country_data_imputed %>% filter(validation %in% c("error","zeros"))

        # valid data for the country
        country_data <- country_data_imputed %>% filter(!validation %in% c("error","zeros"))
        
        # data selection: sex specific ----------------------------------------------------------
        
        selection_gaps_sex <- list()
        for(this_sex in c("m","f")){
                # data selection following criterias
                selected_data <- select_data(country_data = country_data %>% filter(SexName==this_sex))
                
                # detect main time serie and gaps ----------------------------------------------------------
                
                chunks_data <- split(selected_data$TimeMid[selected_data!="WPP19"], 
                                     cumsum(c(1, diff(selected_data$TimeMid[selected_data!="WPP19"]) >= 2))) # !=1
                
                main_data_time <- chunks_data[which(lengths(chunks_data)>5)] %>% unlist(.)
                
                # define intervals of gap: take care of some countries with sparse data points and any consecutive data
                if(!is.null(main_data_time)){
                        years_gaps <- sort(dates_out[which(!(dates_out) %in% (main_data_time))])
                        intervals_gaps <- split(years_gaps, cumsum(c(1, diff(years_gaps) != 1)))
                }else{
                        years_gaps <- sort(dates_out[which(!(dates_out) %in% (selected_data$TimeMid[selected_data!="WPP19"]))])
                        intervals_gaps <- split(years_gaps, cumsum(c(1, diff(years_gaps) != 1)))
                }
                
                # if we dont have a huge first gap (15 years) don´t use wpp19 as a base in 1953
                if(!(first_year+.5) %in% intervals_gaps[[1]] |  
                   ((first_year+.5) %in% intervals_gaps[[1]] & !length(intervals_gaps[[1]])>15)){
                        selected_data <- selected_data %>% filter(DataSourceShortName!="WPP19")
                }
                
                # is each year data part of the main time serie or is an additional data point, as a resource for pivoting in the filling
                selected_data <- selected_data %>% 
                        mutate(Type = ifelse(TimeMid %in% main_data_time, "main", "add"))
                
                # get same cut-off at oldest ages:the mode
                if(any(selected_data$Type=="main")){
                        extrapFrom_single <- Mode(selected_data$OAG[selected_data$Type=="main"])
                }else{
                        extrapFrom_single <- Mode(selected_data$OAG)
                }
                
                # selection data attributes for each sex
                selection_gaps_sex[[this_sex]] <- list(selected_data = selected_data %>% mutate(SexName = this_sex), 
                                                       chunks_data = chunks_data,
                                                       main_data_time = main_data_time,
                                                       years_gaps = years_gaps,
                                                       intervals_gaps = intervals_gaps, 
                                                       extrapFrom_single = extrapFrom_single)  
        }
        
        # keep life table data that was selected
        selected_data <- bind_rows(selection_gaps_sex[["f"]]$selected_data, selection_gaps_sex[["m"]]$selected_data)
        extrapFrom_single <- min(selection_gaps_sex[["f"]]$extrapFrom_single, selection_gaps_sex[["m"]]$extrapFrom_single)
        lt_data_raw <- country_data %>% 
                inner_join(selected_data %>% select(-nrank3,-DataTypeSort,-TimeMid_floor), 
                           by=c("DataSourceShortName","TimeLabel", "IndicatorName","TimeMid","SexName")) %>% 
                filter(!is.na(Type), str_detect(IndicatorName,"x")) %>% 
                mutate(TimeMid_floor = floor(TimeMid)) %>% 
                distinct()
                
        # harmonize all lt data with same close-out ------------------------------------------
        # if is input is Abridged then standarize with same OAG. If not, graduate all to single ages
        Single_first <- ifelse(Age_Specific_Mortality_Input_Data == "abridged", FALSE, TRUE)
        
        # standarize/graduate
        lt_data <-  lt_data_raw %>% 
                split(list(.$TimeMid, .$SexName), drop = T) %>% 
                lapply(function(X){
                        # print(paste0(unique(X$SexName),"-", unique(X$TimeMid)))
                        # X <- lt_data_raw %>% filter(TimeMid==2000.5,SexName=="f")
                        X_TimeMid <- unique(X$TimeMid)
                        X_TimeMid_floor <- unique(X$TimeMid_floor)
                        X_sex <- unique(X$SexName)
                        X_DataProcess <- unique(X$DataProcess)
                        X_ages <- X$AgeStart
                        Indicator_type = ifelse(str_detect(unique(X$IndicatorName),"m\\(x,n"),"mx",
                                                ifelse(str_detect(unique(X$IndicatorName),"l\\(x"),"lx",NA))
                        # detect if it is a real OAG
                        X_OAG <- max(X_ages)
                        if(Indicator_type=="mx" & !unique(X$complete)){
                                N <- length(X_ages)
                                is_an_OAG <- ifelse(X$DataValue[N] > X$DataValue[N-1], TRUE, FALSE)
                        }else{
                                is_an_OAG = TRUE # is the default also in DemoTools, I don´t know how to infer this only with l(x)
                        }
                        print(paste0(unique(X$SexName),"-", unique(X$TimeMid),"- OAG ",is_an_OAG))     
                        # see inputs for adjustments
                        X_inputs <- mort_inputs %>% filter(TimeMid_floor == X_TimeMid_floor, Sex == X_sex)
                        
                        # adjust infant-child if was defined in InputFiles
                        adjust_LT_infant <- ifelse(nrow(X_inputs)==0, F, X_inputs$adjust_LT_under_five)
                        if(adjust_LT_infant){ # only for Abr selection
                                if(Age_Specific_Mortality_Input_Data == "abridged" & !unique(X$complete)){
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
                        
                        # VR coverage adjustment for ages>=5, if was defined in InputFiles
                        # first get always rates - don´t extend
                        LT_cov <- lt_ambiguous(nMx_or_nqx_or_lx = X$DataValue,
                                            type = Indicator_type,
                                            Age = X_ages,
                                            Sex = X_sex,
                                            a0rule = "cd", region = "w",
                                            Single = Single_first,
                                            OAnew = X_OAG)
                        # recover OAG rate just to be sure DemoTools did not impute it
                        if(Indicator_type=="mx"){
                                LT_cov$nMx[LT_cov$Age==X_OAG] <- X$DataValue[LT_cov$Age==X_OAG]
                        }
                        adjust_VR_completeness <- ifelse(is.na(X_DataProcess) | X_DataProcess != "VR" | nrow(X_inputs)==0, 1, 
                                                          as.numeric(X_inputs$VR_completeness))
                        LT_cov$nMx[LT_cov$Age>=5] <- LT_cov$nMx[LT_cov$Age>=5]/ adjust_VR_completeness
                        
                        # Old-age adjustments if was defined in InputFiles
                        adjust_LT_oldage <- ifelse(nrow(X_inputs)==0, F, X_inputs$adjust_LT_oldage)
                        # six groups as minimum data
                        min_age_fit <- sort(X_ages, T)[6]
                        # kind of age interval
                        N_fit <- ifelse(unique(X$complete),1,5)
                        if(adjust_LT_oldage){
                                if(tolower(X_inputs$extrapLaw) %in% mort_laws_options){
                                        X_extrapLaw <- X_inputs$extrapLaw
                                }else{
                                        X_extrapLaw <- "kannisto"
                                        log_print(paste0("Sorry, did not find the mortality law of year", 
                                                         X_TimeMid_floor," of possible options in the MortalityLaws package. Used Kannisto instead."))
                                }
                                X_extrapFrom <- min(X_inputs$extrapFrom, X_OAG)
                                X_range_age_fit <- as.numeric(stringr::str_extract_all(X_inputs$extrapFit, "\\d+")[[1]])
                                X_extrapFit <- seq(min(min_age_fit, X_range_age_fit[1]),
                                                   min(X_OAG, ifelse(is.na(X_range_age_fit[2]),1e3,X_range_age_fit[2]))-
                                                           N_fit, #ifelse(is_an_OAG,N_fit,0),
                                                       N_fit)
                                if(Age_Specific_Mortality_Input_Data == "abridged"){
                                        X_ages_out <- c(0,1,seq(5,OAnew,5))
                                }else{
                                        X_ages_out <- 0:OAnew
                                }
                                # coKannisto needs both sex
                                if(X_extrapLaw == "coKannisto"){
                                        # the other sex
                                        Y <- lt_data_raw %>% filter(TimeMid == X_TimeMid,
                                                                    DataProcess == X_DataProcess,
                                                                    SexName != X_sex)
                                        X_LT <- LT_cov
                                        # give to the other sex the same oag
                                        Y_LT <- lt_ambiguous(nMx_or_nqx_or_lx = Y$DataValue,
                                                             type = Indicator_type,
                                                             Age = Y$AgeStart,
                                                             Sex = unique(Y$SexName),
                                                             a0rule = "cd", region = "w",
                                                             Single = Single_first,
                                                             OAnew = X_OAG)
                                        if(X_sex == "m"){
                                                mxM = X_LT %>% select(nMx)
                                                mxF = Y_LT %>% select(nMx)
                                        }else{
                                                mxM = Y_LT %>% select(nMx)
                                                mxF = X_LT %>% select(nMx)}
                                        rownames(mxM) <- rownames(mxM) <- X_LT$Age
                                        XY_LT <- cokannisto(mxM, mxF,
                                                            est.ages = X_extrapFit, 
                                                            proj.ages = seq(X_extrapFrom,OAnew,N_fit))
                                        if(X_sex == "m"){
                                                X_LT <- as.numeric(XY_LT$male)
                                        }else{
                                                X_LT <- as.numeric(XY_LT$female)
                                        }
                                        # plot(X_ages_out,XY_LT$male, log="y", xlim=c(0,100), col=2, t="l")
                                        # points(X_ages, mxM$nMx, log="y", xlim=c(0,100),col=2)
                                        # points(X_ages, mxF$nMx)
                                        # lines(X_ages_out,XY_LT$female)
                                        LT <- lt_single_mx(X_LT, Age = X_ages_out)
                                }else{
                                        # if not coKannisto
                                        LT <- lt_ambiguous(nMx_or_nqx_or_lx = LT_cov$nMx,
                                                           type = "mx",
                                                           Age = LT_cov$Age,
                                                           Sex = X_sex,
                                                           a0rule = "cd", region = "w",
                                                           extrapLaw = X_extrapLaw,
                                                           extrapFrom = X_extrapFrom,
                                                           extrapFit = X_extrapFit,
                                                           OAnew = OAnew,
                                                           Single = Single_first,
                                                           OAG = is_an_OAG)      
                                }
                        }else{
                                # if not adjustment on old ages
                                X_extrapLaw <- NULL
                                X_extrapFrom <- min(extrapFrom_single,max(X$AgeStart))
                                # es muy peligroso tomar el último grupo, ha dado resultado locos: URU 2002 females
                                X_extrapFit <- seq(min(min_age_fit,60),max(X$AgeStart)-
                                                           N_fit, #ifelse(is_an_OAG,N_fit,0),
                                                   N_fit)
                                LT <- lt_ambiguous(nMx_or_nqx_or_lx = LT_cov$nMx,
                                                   type = "mx",
                                                   Age = LT_cov$Age,
                                                   Sex = X_sex,
                                                   a0rule = "cd", region = "w",
                                                   extrapLaw = X_extrapLaw,
                                                   extrapFrom = X_extrapFrom,
                                                   extrapFit = X_extrapFit,
                                                   OAnew = OAnew,
                                                   Single = Single_first,
                                                   OAG= is_an_OAG)
                                # conservative extension: constrained in case 1/M(OAG) is less than DemoTools extension  
                                if(!unique(X$complete) & !Single_first & is_an_OAG & X_OAG<90){
                                        eOAG_DemoTools_ext = LT$ex[LT$Age==X_OAG]
                                        eOAG_classical <- LT_cov$ex[LT_cov$Age==X_OAG]
                                        if(eOAG_classical < (eOAG_DemoTools_ext-.5)){
                                                browser()
                                                # constrained extension
                                                LT <- lt_extrap_constrained(LT_cov$nMx, 
                                                                            Age = LT_cov$Age, 
                                                                            Sex = X_sex, 
                                                                            OAnew = OAnew, 
                                                                            extrapLaw = "Kannisto", 
                                                                            Single = F)
                                                eOAG_constrained_ext = LT$ex[LT$Age==X_OAG]
                                        print(round(c(eOAG_classical, eOAG_DemoTools_ext, eOAG_constrained_ext),1)) 
                                        }
                                }
                        }

                        # final vars
                        LT$Date <- as.numeric(unique(X$TimeMid))
                        LT$Type <- unique(X$Type)
                        LT$Source <- unique(X$DataSourceShortName)
                        LT$Sex  <- X_sex
                        LT$DataProcess <- X_DataProcess
                        
                        # if last age is NA (some DemoTools issue with super rates) set to 50% more than previous
                        LT$nMx[LT$Age==100 & is.na(LT$nMx)] <- LT$nMx[LT$Age==(100-N_fit)] * 1.5
                        
                        return(LT)
                }) %>% 
                bind_rows()
        
        # keep for comparing in a plot: adj and not adj on oldest ages
        old_age_adj_data <- lt_data
        
        # smooth in case is abr
        if(Age_Specific_Mortality_Input_Data == "abridged" & Empirical_LT_smoothing_input != "none"){
                # get structure
                lt_data_smooth <- lt_data %>% mutate(nMx_smooth = NA) %>% filter(Age==-1) 
                # redefine length left/right observations
                window_smooth <- as.integer(str_extract_all(Empirical_LT_smoothing_input, "\\d+")[[1]])
                k_smooth <- ifelse(window_smooth==7,3,ifelse(window_smooth==5, 2, 1))
                for(this_sex in c("f","m")){
                        # this_sex = "f"
                        for(chunk in selection_gaps_sex[[this_sex]]$chunks_data){
                                # chunk <- selection_gaps_sex[[this_sex]]$chunks_data$`2`
                                lt_data_smooth_sex <- lt_data %>% 
                                        filter(Date %in% chunk, Sex == this_sex) %>% 
                                        group_by(Sex,Age) %>% 
                                        arrange(Sex, Age, Date) %>% 
                                        mutate(nMx_smooth = slide(nMx, mean, .before = k_smooth, .after = k_smooth) %>% unlist()) %>% 
                                        ungroup()
                                # take out corners
                                n_chunk <- length(chunk)
                                # 3-years
                                if(k_smooth == 1 & n_chunk==2){
                                        lt_data_smooth_sex_chunk <- lt_data_smooth_sex %>% 
                                                filter(Date %in% max(chunk)) %>% 
                                                mutate(Date = mean(chunk))
                                }else if(k_smooth == 1 & n_chunk>2){
                                        lt_data_smooth_sex_chunk <- lt_data_smooth_sex %>% 
                                                filter(!Date %in% range(chunk))
                                # 5-years
                                }else if(k_smooth == 2 & n_chunk %in% 2:4){
                                        lt_data_smooth_sex_chunk <- lt_data_smooth_sex %>% 
                                                filter(Date %in% chunk[2]) %>% 
                                                mutate(Date = chunk[2])
                                }else if(k_smooth == 2 & n_chunk>=5){
                                        lt_data_smooth_sex_chunk <- lt_data_smooth_sex %>% 
                                                filter(!Date %in% chunk[c(1,2,n_chunk-1,n_chunk)])
                                # 7-years
                                }else if(k_smooth == 3 & n_chunk %in% 2:6){
                                        lt_data_smooth_sex_chunk <- lt_data_smooth_sex %>% 
                                                filter(Date %in% chunk[3]) %>% 
                                                mutate(Date = mean(chunk))
                                }else if(k_smooth == 3 & n_chunk>=7){
                                        lt_data_smooth_sex_chunk <- lt_data_smooth_sex %>% 
                                                filter(!Date %in% chunk[c(1,2,3,n_chunk-2,n_chunk-1,n_chunk)])
                                }else{
                                        lt_data_smooth_sex_chunk <- lt_data_smooth_sex
                                }
                                lt_data_smooth <- rbind(lt_data_smooth, lt_data_smooth_sex_chunk)
                        }
                }
                lt_data <- lt_data_smooth %>%
                        arrange(Sex, Date, Age) %>% 
                        split(list(.$Date, .$Sex), drop = T) %>% 
                        lapply(function(X){
                                print(paste0(unique(X$Sex),"-", unique(X$Date)))
                                # X <- lt_data_smooth %>% filter(Date==1980.5,Sex=="f")
                                LT <- lt_abridged(nMx = X$nMx_smooth, Age = X$Age, a0rule = "cd", region = "w", Sex = unique(X$Sex)) 
                                LT$Date <- X$Date
                                LT$Type <- unique(X$Type)
                                LT$Source <- unique(X$Source)
                                LT$Sex  <-  unique(X$Sex)
                                LT$DataProcess <- unique(X$DataProcess)
                                return(LT)
                        }
                        ) %>% 
                        bind_rows() %>% 
                        bind_rows(old_age_adj_data %>% filter(Source == "WPP19")) %>% 
                        arrange(Date,Sex,Age)
                # redefine chunks of selected data
                selected_data_smooth <- lt_data %>% distinct(Date,Sex,Source)
                for(this_sex in c("m","f")){
                        # detect main time serie and gaps
                        selected_data_smooth_sex <- selected_data_smooth %>% filter(Sex == this_sex)
                        chunks_data <- split(selected_data_smooth_sex$Date[selected_data_smooth_sex$Source!="WPP19"], 
                                             cumsum(c(1, diff(selected_data_smooth_sex$Date[selected_data_smooth_sex$Source!="WPP19"]) >= 2)))
                        main_data_time <- chunks_data[which(lengths(chunks_data)>5)] %>% unlist(.)
                        years_gaps <- sort(dates_out[which(!(dates_out) %in% (main_data_time))])
                        intervals_gaps <- split(years_gaps, cumsum(c(1, diff(years_gaps) != 1)))
                        selection_gaps_sex[[this_sex]]$chunks_data <- chunks_data
                        selection_gaps_sex[[this_sex]]$main_data_time <- main_data_time 
                        selection_gaps_sex[[this_sex]]$years_gaps <- years_gaps
                        selection_gaps_sex[[this_sex]]$intervals_gaps <- intervals_gaps
                }
                # redefine what is main because corners were cutted
                lt_data <- lt_data %>% select(-Type) %>% 
                        left_join(bind_rows(
                                tibble(Sex = "f", Date = selection_gaps_sex[["f"]]$main_data_time, Type = "main"),
                                tibble(Sex = "m", Date = selection_gaps_sex[["m"]]$main_data_time, Type = "main")),
                                by = c("Date", "Sex")) %>% 
                        mutate(Type = ifelse(is.na(Type), "add", Type))
        }
        
        # include MLT in case it is asked
        if(!is.null(mlts)){
                if(Age_Specific_Mortality_Input_Data == "abridged"){
                        #abreviate
                        MLT_data <- mlts %>% 
                                split(list(.$Date, .$Sex), drop = T) %>% 
                                lapply(function(X){
                                        LT130 <- lt_single2abridged(nMx = X$nMx,
                                                                 lx = X$lx,
                                                                 nLx = X$nLx,
                                                                 ex = X$ex, 
                                                                 Age = X$Age, 
                                                                 Sex = unique(X$Sex))
                                        LT <- lt_abridged(nMx = LT130$nMx,
                                                          Age = LT130$Age, 
                                                          Sex = unique(LT130$Sex),
                                                          OAnew = OAnew)
                                        LT$Sex <- unique(X$Sex)
                                        LT$Date <- as.numeric(unique(X$Date))
                                        LT$Source <- "MLT"
                                        LT$Type <- "add"
                                        LT$Sex  <- unique(X$Sex)
                                        LT$DataProcess <- NA
                                        LT
                                }) %>% bind_rows()
                }else{
                        MLT_data <- mlts %>%  mutate(Source = "MLT", Dataprocess = NA, Type = "add")
                }
        # MLT inlcusion replaces everything in that same date
        lt_data <- lt_data %>% 
                filter(!Date %in% unique(MLT_data$Date)) %>% 
                bind_rows(MLT_data)
        MLT_data_selection <- MLT_data %>% 
                transmute(IndicatorName = "m(x,n) - abridged",
                          TimeMid = Date,
                          DataSourceShortName = "MLT",
                          Type = "add",
                          SexName = Sex)
        selected_data <- bind_rows(selected_data,MLT_data_selection %>% distinct()) %>% arrange(TimeMid,SexName)
        country_data <- bind_rows(selected_data,MLT_data_selection) %>% arrange(TimeMid,SexName)
        }
        
        # loop over gaps and estimate a LC model -----------------------------------
        # fill depending if the gaps are the same for both sex
        if(all(selection_gaps_sex[["f"]]$years_gaps==selection_gaps_sex[["m"]]$years_gaps)){
                intervals_gaps <- selection_gaps_sex[["f"]]$intervals_gaps
                main_data_time <- selection_gaps_sex[["f"]]$main_data_time
                if(length(intervals_gaps[[1]])!=0){
                        final_data <- fill_intervals_gaps(intervals_gaps, main_data_time,
                                                          lt_data, 
                                                          first_year, last_year,
                                                          Empirical_LT_LeeCarter_time_window_fit_Limited_LC,
                                                          Empirical_LT_LeeCarter_time_window_fit_LC,
                                                          Empirical_LT_LeeCarter_non_divergent,
                                                          Empirical_LT_LeeCarter_jump_off,
                                                          Empirical_LT_LeeCarter_fit_e0,
                                                          Single = Single_first)        
                }else{
                        final_data <- lt_data
                }
        }else{
                intervals_gaps_f <- selection_gaps_sex[["f"]]$intervals_gaps
                main_data_time_f <- selection_gaps_sex[["f"]]$main_data_time
                dates_this_sex <- lt_data %>% filter(Sex=="f") %>% count(Date) %>% pull(Date)
                if(length(intervals_gaps_f)!=0){
                        final_data_f <- fill_intervals_gaps(intervals_gaps_f, main_data_time_f,
                                                          rbind(lt_data %>% filter(Sex=="f"),
                                                                lt_data %>% filter(Sex=="f") %>% mutate(Sex="m")) %>% 
                                                                filter(Date %in% dates_this_sex), 
                                                          first_year, last_year,
                                                          Empirical_LT_LeeCarter_time_window_fit_Limited_LC,
                                                          Empirical_LT_LeeCarter_time_window_fit_LC,
                                                          Empirical_LT_LeeCarter_non_divergent = FALSE,
                                                          Empirical_LT_LeeCarter_jump_off,
                                                          Empirical_LT_LeeCarter_fit_e0,
                                                          Single = Single_first) %>% filter(Sex=="f")   
                }else{
                        final_data_f <- lt_data %>% filter(Sex=="f")
                }
                intervals_gaps_m <- selection_gaps_sex[["m"]]$intervals_gaps
                main_data_time_m <- selection_gaps_sex[["m"]]$main_data_time
                dates_this_sex <- lt_data %>% filter(Sex=="m") %>% count(Date) %>% pull(Date)
                if(length(intervals_gaps_m)!=0){
                        final_data_m <- fill_intervals_gaps(intervals_gaps_m, main_data_time_m,
                                                          rbind(lt_data %>% filter(Sex=="m"),
                                                                lt_data %>% filter(Sex=="m") %>% mutate(Sex="f")) %>% 
                                                          filter(Date %in% dates_this_sex), 
                                                          first_year, last_year,
                                                          Empirical_LT_LeeCarter_time_window_fit_Limited_LC,
                                                          Empirical_LT_LeeCarter_time_window_fit_LC,
                                                          Empirical_LT_LeeCarter_non_divergent = FALSE,
                                                          Empirical_LT_LeeCarter_jump_off,
                                                          Empirical_LT_LeeCarter_fit_e0,
                                                          Single = Single_first)  %>% filter(Sex=="m")      
                }else{
                        final_data_m <- lt_data %>% filter(Sex=="m")
                }
                final_data <- rbind(final_data_f, final_data_m)
        }
        
        # get only dates_out
        final_data <- final_data %>% filter(Date %in% dates_out)
        
        # graduate to complete in case is abr -----------------------------------------------------------------------

        if(Age_Specific_Mortality_Input_Data == "abridged"){
                final_data_abr <- final_data %>% 
                        split(list(.$Sex, .$Date), drop = T) %>% 
                        lapply(function(X){
                                print(paste0(unique(X$Sex),"-", unique(X$Date)))
                                # X <- final_data %>% filter(Date==2016.5,Sex=="f")
                                X_sex <- unique(X$Sex)
                                X_ages <- X$Age
                                # for some reason realted to initial values in optimization, some few country-year-sex abridged lt don´t converge. Try 5 times. The last one with Makeham
                                LT <- NULL
                                attempt <- 1
                                while(is.null(LT) && attempt<=7) {
                                        if(attempt %in% 1:5){
                                                try(
                                                        # using default option in DemoToools
                                                        LT <- lt_abridged2single(nMx = X$nMx,
                                                                                 Age = X_ages,
                                                                                 Sex = X_sex,
                                                                                 a0rule = "cd", region = "w",)
                                                )
                                        }else if(attempt==6){
                                                try(
                                                        # use no default method to extend
                                                        LT <- lt_abridged2single(nMx = X$nMx,
                                                                             Age = X_ages,
                                                                             Sex = X_sex,
                                                                             a0rule = "cd", region = "w",,
                                                                             extrapLaw = "Makeham")
                                                        )
                                                log_print(paste0("Warning: Year ",unique(X$Date),
                                                                 " possibly with input data problems. Take a look and remove if is neccesary."))
                                        }else{
                                                        # extreme case: at least return something and decide later
                                                        Age_output <- 0:100
                                                        lx_smooth <- data.frame(Age = Age_output, 
                                                                                lx = splinefun(x = X_ages, 
                                                                                               y = X$lx, 
                                                                                               method = "monoH.FC")(Age_output))
                                                try(
                                                        LT <- lt_single_qx(nqx = lt_id_l_q(lx_smooth$lx),Age = Age_output, 
                                                                           Sex = X_sex, 
                                                                           # a0rule = "cd", region = "w",
                                                                           OAnew = OAnew)        
                                                )
                                        }
                                        attempt <- attempt + 1
                                }
                                # if is not possible to graduate
                                if(is.null(LT)){
                                        stop(paste0("Error with year ",unique(X$Date),". It is very irregular for graduating to complete."))
                                        log_print(paste0("Error with year ",unique(X$Date),". It is very irregular for graduating to complete."))
                                }
                                LT$Date <- as.numeric(unique(X$Date))
                                LT$Type <- unique(X$Type)
                                LT$Source <- unique(X$Source)
                                LT$DataProcess <- unique(X$DataProcess)
                                LT$Sex  <- X_sex
                                return(LT)
                        }) %>% 
                        bind_rows()
                final_data <- final_data_abr %>%
                        arrange(Date, Sex, Age)
        }

        # plan B adjustment old ages: ratio method ----------------------------
        # dates_adj = 1950:1964; min_age_adj = 60
        dates_adj <- NULL
        if(!is.null(dates_adj)){
                final_data <- old_age_adj(final_data, dates_adj, min_age_adj) %>%
                        arrange(Date, Sex, Age)
        }

        # output smoothing -----------------------------------------------------

        final_data_smooth <- final_data  %>%
                arrange(Date, Sex, Age) %>% 
                split(.$Sex) %>% 
                lapply(function(X){
                        # X <- final_data %>% filter(Sex=="m")
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
                        # exposed from wpp19: only to 2019. take 2020 as same just for smoothing
                        E <- pop %>%
                                select(Age=AgeStart, Date=Year, E=this_sex) %>% 
                                mutate(E = E * 1000) %>% 
                                arrange(Date,Age) %>% 
                                pivot_wider(names_from = Date, values_from = E)   %>% 
                                select(-Age) %>% 
                                mutate(`2020.5` = `2019.5`) %>% as.matrix()
                        # add 1 exposure in case 0
                        E[E==0] <- 2 
                        # implicit deaths
                        if(!all(dim(E)==dim(M))){
                                log_print(paste0("The sex ",X_sex," comes to output smoothing with less years that it should. Some data issue must be checked."))
                                stop(paste0("The sex ",X_sex," comes to output smoothing with less years that it should. Some data issue must be checked."))
                        }
                        D <- M * E
                        # model options
                        # StMoMo - APC model
                        APC <- StMoMo::apc()
                        APCfit <- try(StMoMo::fit(APC, Dxt =D, Ext = E, ages = X_Ages, 
                                              years=X_Years, verbose = FALSE))
                        if("try-error" %in% class(APCfit)){
                                M_smooth_APC <- cbind(M[,1],t(apply(M,1,zoo::rollmean,k=3)),M[,ncol(M)])
                                colnames(M_smooth_APC) <- as.character(dates_out)
                                log_print(paste0("StMomo gave an error because of irregular mortality patterns by age in sex ",X_sex,
                                                 ". It was reaplaced by a 3-year moving average on time"))
                        }else{
                                M_smooth_APC <- fitted(APCfit, type = "rates") 
                        }
                        M_hat_APC <- M_smooth_APC %>% 
                                        as.data.frame() %>% 
                                        mutate(Age = X_Ages) %>% 
                                        pivot_longer(cols=-Age, names_to="Date",values_to="nMx") %>% 
                                        mutate(Sex = X_sex)
                        LT_hat_APC <- M_hat_APC %>% split(.$Date) %>% 
                                        lapply(function(Y){
                                                # Y = M_hat_APC %>% filter(Date == 1950.5)
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
                        if("try-error" %in% class(fit2D)){
                                fit2D <- log(cbind(M[,1],t(apply(M,1,zoo::rollmean,k=3)),M[,ncol(M)]))
                                M_smooth_MS <- exp(fit2D)
                                colnames(M_smooth_MS) <- as.character(dates_out)
                                log_print(paste0("MortalitySmooth gave an error because of irregular mortality patterns by age in sex ",X_sex,
                                          ". It was reaplaced by a 3-year moving average on time"))
                        }else{
                                M_smooth_MS <- exp(fit2D$logmortality)
                        }
                        min_rate <- min(lt_data$nMx)/2
                        M_hat_MS <- M_smooth_MS %>% 
                                as.data.frame() %>% 
                                mutate(Age = X_Ages) %>% 
                                pivot_longer(cols=-Age, names_to="Date",values_to="nMx") %>% 
                                mutate(Sex = X_sex, nMx = pmax(min_rate,nMx))
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
        if(Empirical_LT_smoothing_output == "apc" | isTRUE(Empirical_LT_smoothing_output)){     
                final_LT <- final_data_smooth %>% filter(Model == "APC") %>% select(-Model)
        }else if(Empirical_LT_smoothing_output == "ap"){
                final_LT <- final_data_smooth %>% filter(Model == "AP") %>% select(-Model)
        }else{
                final_LT <- final_data
        }

        # add mort crisis on the top ---------------------------------------------------------
        mort_inputs$adjust_crisis_true <- as.logical(pmin(1,
                                mort_inputs$adjust_conflict_mortality+
                                mort_inputs$adjust_disasters_mortality+
                                mort_inputs$adjust_COVID19_mortality))
        
        if(!is.na(mort_crises$time_start[1])){
                final_data_mc <- final_LT %>% 
                        left_join(mort_crises %>%
                                          left_join(mort_inputs %>% select(time_start, sex, adjust_crisis_true),
                                                    by = c("time_start", "sex")) %>% 
                                          filter(adjust_crisis_true) %>% 
                                          select(Date=TimeMid, Age=age_start, Sex=Sex, value),
                                  by = c("Age", "Date", "Sex")) %>% 
                        split(list(.$Date, .$Sex), drop = T) %>% 
                        lapply(function(X){
                                # print(paste0(unique(X$Sex),"-", unique(X$Date)))
                                # X <- pepe %>% filter(Date==2016.5,Sex=="f")
                                X_sex <- unique(X$Sex)
                                X$value <-  ifelse(is.na(X$value), 0, X$value)
                                X$nMx_adj <- X$nMx + X$value
                                LT <- lt_single_mx(X$nMx_adj, X$Age, Sex = unique(X$Sex), extrapFrom = OAnew)
                                LT$Date <- as.numeric(unique(X$Date))
                                LT$Source <- unique(X$Source)
                                LT$Sex  <- unique(X$Sex)
                                return(LT)
                        }) %>% bind_rows()    
        }else{
                final_data_mc <- final_LT
        }
        
        
        # outputs -----------------------------------------------------------------
        
        # recover IDseries
        selected_data_series <- selected_data %>% 
                left_join(country_data %>% 
                        distinct(TimeLabel, IndicatorID, IndicatorName, SeriesID, DataSourceShortName),
                          by = c("DataSourceShortName", "TimeLabel", "IndicatorName"))
        
        # abridged output
        final_data_mc_abr <- final_data_mc %>% 
                split(list(.$Date, .$Sex), drop = T) %>% 
                lapply(function(X){
                        LT <- lt_single2abridged(nMx = X$nMx,
                                                     lx = X$lx,
                                                     nLx = X$nLx,
                                                     ex = X$ex, 
                                                     Age = X$Age, 
                                                     Sex = unique(X$Sex))
                        LT$Sex <- unique(X$Sex)
                        LT$Date <- as.numeric(unique(X$Date))
                        LT$Source <- unique(X$Source)
                        LT$Sex  <- unique(X$Sex)
                        LT
                }) %>% bind_rows()

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
        
        life_table_age_sex_abridged <- 
                final_data_mc_abr %>% 
                pivot_longer(cols = nMx:ex,names_to = "indicator", values_to = "value") %>% 
                mutate(indicator = paste0("lt_",indicator),
                       sex = ifelse(Sex == "f", "female", "male"),
                       time_start = trunc(Date),
                       time_span = 1,
                       age_span = ifelse(Age == 0, 1, 
                                         ifelse(Age==1, 4, 
                                                ifelse(Age==OAnew, 1000, 5)))) %>% 
                select(indicator,time_start,time_span,sex,age_start=Age,age_span,value) %>% 
                arrange(indicator, time_start, sex, age_start)
        
        dd_selected_series <- selected_data_series %>% 
                mutate(Status = "Used", SeriesKey = as.character(SeriesID)) %>% 
                select(IndicatorID, SeriesKey, Status)
        
        # final output
        out <- list(
                name = country_full,
                input_data = country_data,
                old_age_adj_data = old_age_adj_data,
                selected_data = selected_data_series, 
                output_data = final_data_mc,
                output_data_abr = final_data_mc_abr,
                data_pre_smooth = final_data, 
                data_with_smooth = final_data_smooth, 
                error_data = country_data_errors,
                gaps = intervals_gaps,
                life_table_age_sex = life_table_age_sex,
                life_table_age_sex_abridged = life_table_age_sex_abridged,
                dd_selected_series = dd_selected_series)
        return(out)
}
