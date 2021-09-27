# author: IW
# function called from `main.R`. Read arguments from InputFiles, download/use the country data from server,
# build the time series, write InputFiles and make the plots.

fill_gaps <- function(country = NULL, 
                      dirs = NULL,
                      dates_out = 1950.5:2020.5,
                      OAnew = 100,
                      first_year = 1950,
                      last_year = 2020){
        
        # read inputs
        mort_params <- read_excel(path=dirs$InputFiles_dir, sheet = "MORT_PARAMS", col_names = TRUE) %>% 
                filter(type == "Estimation") %>% 
                pivot_wider(names_from=parameter,values_from=value) %>% 
                as.list()
        mort_inputs <- read_excel(path=dirs$InputFiles_dir, sheet = "MORT_INPUTS", col_names = TRUE) %>% 
                mutate(TimeMid = time_start + .5,
                       TimeMid_floor = trunc(TimeMid),
                       Sex = ifelse(sex=="male","m","f"))
        mort_crises <- read_excel(path=dirs$InputFiles_dir, sheet = "mx1_crises", col_names = TRUE) %>% 
                mutate(TimeMid = time_start + .5,
                       TimeMid_floor = trunc(TimeMid),
                       Sex = ifelse(sex=="male","m","f"))
        
        # build lt series: using empirical data
        if(mort_params$Age_Specific_Mortality_Type == "Empirical"){
                
                # read short notes
                
                # get data. The indicators depends on type of data asked (complete/abridged)
                indicators_lt_complete <- c(255,256,245,246) 
                indicators_lt_abridged <- c(255,245) 
                if(mort_params$Age_Specific_Mortality_Input_Data == "Abridged"){
                        indicators_lt <- indicators_lt_abridged
                }else{
                        indicators_lt <- indicators_lt_complete
                }
                
                if(file.exists(file.path(dirs$aux_dir, paste0(country$Name,".csv")))){
                        country_db <- read.csv(file.path(dirs$aux_dir, paste0(country$Name,".csv"))) %>% 
                                filter(IndicatorID %in% indicators_lt)
                        
                }else{
                        country_db <- download_db_reading_short_notes(myLocID = as.integer(country$PK_LocID),
                                                                      myIndicators = indicators_lt)
                        # country_db <- get_recorddata(
                        #         dataProcessTypeIds = c(6, 7, 9, 10), # 6=Estimate; 7=Life Table (legacy UN DYB) ; 9=Register ; 10=Sample Registration System (SRS);                                              startYear = 1950,
                        #         endYear = 2020,
                        #         indicatorIds = indicators_lt,
                        #         locIds = as.integer(country$PK_LocID),
                        #         locAreaTypeIds = 2,
                        #         subGroupIds = 2,
                        #         includeUncertainty = FALSE,
                        #         collapse_id_name = FALSE)  
                        write.csv(country_db, file.path(dirs$aux_dir, paste0(country$Name,".csv")), row.names = F)
                }
                
                # include classic model life tables in case is needed (taken from Sara Hertog´s code) -----------------------------
                # read the mortality estimation parameters
                mort_models <- readxl::read_xlsx(path = dirs$InputFiles_dir, sheet = "MORT_MODELS")
                model_region <- mort_params$Age_Specific_MLT_Region
                model_dates_selected <- NULL
                # model_dates_selected <- 1950:1969 + .5
                if(length(model_dates_selected)>0){
                        if (model_region %in% c("CD_West","CD_East","CD_North","CD_South", "UN_Chilean","UN_Far_Eastern",
                                                "UN_General","UN_Latin_American","UN_South_Asian")) {
                                
                                mlts <- mortality_patterns_classicMLT(model_region = model_region,
                                                                      inputs_type = mort_params$Age_Specific_MLT_Type_of_Inputs,
                                                                      model_inputs = mort_models[, c("time_start","sex","q1_value","q5_value","q1545_value","e0_value")]) %>%
                                        bind_rows(.,.id = 'Sex') %>%
                                        mutate(Sex = substr(Sex,1,1),
                                               Date = time_start + .5) %>%
                                        select(-time_start) %>% 
                                        filter(Date %in% model_dates_selected)
                                
                       
                }}else{
                                mlts <- NULL 
                }
                
                # results
                output <- fill_gaps_lt(country_db,
                                          country, 
                                          dates_out,
                                          OAnew = 100,
                                          mort_params,
                                          mort_inputs,
                                          mort_crises,
                                          mlts,
                                          first_year,
                                          last_year)

                # warning: covid added and vr 2020
                if(2020 %in% mort_inputs$time_start[mort_inputs$adjust_COVID19_mortality] & 
                   2020.5 %in% output$selected_data$TimeMid){
                        log_print("Careful: COVID-19 excess mortality was added at the top of VR/Estimate data for 2020.")
                }
                
                # write InputFile with results
                write_InputFile(input.file = dirs$InputFiles_dir, 
                                life_table_age_sex = output$life_table_age_sex,
                                life_table_age_sex_abridged = output$life_table_age_sex_abridged,
                                mySeries = output$dd_selected_series)
                
                # do plots with results
                write_plots(dir_plots = dirs$plots_dir, 
                            output = output, 
                            smoothing = mort_params$Empirical_LT_smoothing_output,
                            dir_HMD = dirs$aux_dir,
                            complete_abridged = mort_params$Age_Specific_Mortality_Input_Data,
                            mort_params, mort_inputs)
                
                # save list of data results
                save(output, file = file.path(dirs$aux_dir,paste0(country$Code_iso, "_output.Rdata")))
                
        }else{
                # model - based
                log_print("Modelled LT. Not here.")
                print("Modelled LT. Not here.")
                
        }
        
        # end: no output
}
