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
                        country_db <- get_recorddata(
                                dataProcessTypeIds = c(6, 7, 9, 10), # 6=Estimate; 7=Life Table (legacy UN DYB) ; 9=Register ; 10=Sample Registration System (SRS);                                              startYear = 1950,
                                endYear = 2020,
                                indicatorIds = indicators_lt,
                                locIds = as.integer(country$PK_LocID),
                                locAreaTypeIds = 2,
                                subGroupIds = 2,
                                includeUncertainty = FALSE,
                                collapse_id_name = FALSE)  
                        write.csv(country_db, file.path(dirs$aux_dir, paste0(country$Name,".csv")), row.names = F)
                }
                
                # results
                output <- fill_gaps_lt(country_db,
                                          country, 
                                          dates_out,
                                          OAnew = 100,
                                          mort_params,
                                          mort_inputs,
                                          mort_crises,
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
                            complete_abridged = mort_params$Age_Specific_Mortality_Input_Data)
                
                # save list of data results
                save(output, file = file.path(dirs$aux_dir,paste0(country$Code_iso, "_output.Rdata")))
                
        }else{
                log_print("Modelled LT. Not here.")
                print("Modelled LT. Not here.")
                
        }
        
        # end: no output
}
