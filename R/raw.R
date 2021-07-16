# # pervio ----------------------------------------------------------
# 
# # get best complete data: HMD or EuroStats
# single_data_available <- country_data %>% 
#         filter(IndicatorName %in% c("m(x,n) - complete"))
# 
# single_availability_source <- single_data_available %>% 
#         filter(DataSourceShortName %in% c("HMD","EuroStat","HLD 2020")) %>% 
#         group_by(DataSourceShortName) %>%
#         summarise(min_t = min(TimeMid), max_t = max(TimeMid), 
#                   interval = max_t-min_t, n = n_distinct(TimeMid))
# 
# # criteria: wider interval single data
# single_selection_source <- single_availability_source %>% 
#         arrange(desc(n),desc(interval)) %>%  
#         slice(1) %>% 
#         pull(DataSourceShortName)
# 
# # data from selected source
# single_data <- single_data_available %>% 
#         filter(DataSourceShortName == single_selection_source) %>% 
#         mutate(Type = "main")
# 
# # detect interval gaps -----------------------------------------------
# single_years_gap <- sort(dates_out[which(!dates_out %in% unique(single_data$TimeMid))])
# 
# single_intervals_gap <- split(single_years_gap, cumsum(c(1, diff(single_years_gap) != 1)))
# 
# if(2020.5 %in% single_intervals_gap[[1]]){
#         names(single_intervals_gap) <- "after"
#         }else{
#                 names(single_intervals_gap) <- c("before","after")
#         }
# 
# # get extra data ----------------------------------------------------------
# # get useful extra data - TODO: a herarchy filter here, taking WPP in case no HLD
# other_lt_data_sources <- country_data %>%
#         distinct(DataSourceShortName,IndicatorName) %>% 
#         filter(str_detect(IndicatorName,"x"),
#                DataSourceShortName!=single_selection_source) %>% 
#         left_join(source_herarchy, "DataSourceShortName") %>% 
#         distinct(DataSourceShortName,index) %>% 
#         arrange(index) %>% 
#         pull(DataSourceShortName) 
#         
# # range of extra single main data
# if(is.null(single_intervals_gap[["before"]])){
#         range_extra_data_before = c(0,0)
# }else{
#         range_extra_data_before <- c(1940,max(single_intervals_gap[["before"]])+.5)
# }
# if(is.null(single_intervals_gap[["after"]])){
#         range_extra_data_after = c(0,0)
# }else{
#         range_extra_data_after <- c(min(single_intervals_gap[["after"]])-.5,max(single_intervals_gap[["after"]])+.5)
# }
# 
# # not selected lt data (single or abr) in the gap ranges
# other_lt_data <- country_data %>% 
#         filter(DataSourceShortName %in% other_lt_data_sources) %>% 
#         filter(between(TimeMid,range_extra_data_before[1],range_extra_data_before[2]) |
#                between(TimeMid,range_extra_data_after[1],range_extra_data_after[2])) %>% 
#         mutate(Type = "add")  
#         # %>% filter(!(DataSourceShortName=="HLD 2020" & TimeLabel == "1960")) # for Greece only, make distinct
# 
# # data for model tables uses
# other_model_data <- country_data %>% 
#         filter(!str_detect(IndicatorName,"x")) %>% 
#         filter(TimeMid %in% single_years_gap)
# 
# # harmonize all lt data to single ages with OAG=100 ------------------------------------------
# 
# # same cut-off at oldest ages
# extrapFrom_single <- max(single_data$AgeStart)
# 
# # lts
# lt_data <-  bind_rows(single_data, other_lt_data) %>% 
#         split(list(.$TimeMid, .$SexName)) %>% 
#         lapply(function(X){
#                 LT_sex <- unique(X$SexName)
#                 if(is_single(unique(X$AgeStart[1:101]))){
#                         X <- X %>% filter(IndicatorName == "m(x,n) - complete")
#                         LT <- lt_single_mx(nMx = X$DataValue,
#                                            Age = X$AgeStart,
#                                            Sex = LT_sex,
#                                            extrapFrom = min(extrapFrom_single,
#                                                             max(X$AgeStart)), 
#                                            OAnew = 100)
#                 }
#                 if(!is_single(unique(X$AgeStart[1:101]))){
#                         X <- X %>% filter(IndicatorName == "l(x) - abridged",
#                                           AgeStart %in% c(0,1,seq(5,105,5))) 
#                         LT <- lt_abridged2single(lx =  X$DataValue,
#                                                  Age = X$AgeStart,
#                                                  Sex = LT_sex,
#                                                  extrapFrom = min(extrapFrom_single,
#                                                                   max(X$AgeStart)),
#                                                  OAnew = 100)
#                 }        
#                 LT$Date <- unique(X$TimeMid)
#                 LT$Type <- unique(X$Type)
#                 LT$Sex  <- LT_sex
#                 LT
#         }) %>% 
#         do.call(rbind,.)
# 
# # plot harmonized sigle lt
# plot_age_time(lt_data)
# plot_ex_time(lt_data)
# 
# # before -----------------------------------
# gap_before <- single_intervals_gap[["before"]]
# 
# # get structure
# gap_before_data <- gap_before_data_fit_e0 <- lt_data %>% filter(Age==-1)
# 
# if(!is.null(gap_before)){
#         
#         # pick years for LC lim - TODO: extrapolate from last observed
#         window <- 2
#         LClim_data <- lt_data %>% 
#                 filter(Date > 1940, Date<(min(lt_data$Date[lt_data$Type=="main"])+window))%>% 
#                 mutate(Date = as.numeric(Date)) %>% 
#                 arrange(Date,Age)
#         
#         # apply LC lim - fit e0
#         dates_e0 <- lt_data %>% filter(Type!="main", Age==0) %>% select(Date) %>% unique() %>% pull()
#         e0_Males <- lt_data %>% filter(Type!="main", Age==0, Sex=="m") %>% select(ex) %>% pull()
#         e0_Females <- lt_data %>% filter(Type!="main", Age==0, Sex=="f") %>% select(ex) %>% pull()
#         gap_before_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap_before),
#                                          Single = T)$lt_hat %>% 
#                 mutate(Type = "LC-Lim")
#         gap_before_data_fit_e0 <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap_before), 
#                                                 dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
#                                                 Single = T)$lt_hat %>% 
#                 mutate(Type = "LC-Lim-fit_e0") 
# }
# 
# # after -------------------------------------------------------------------
# gap_after <- single_intervals_gap[["after"]]
# 
# # get structure
# gap_after_data <- lt_data %>% filter(Age==-1)
# 
# if(!is.null(gap_after)){
#         
#         # pick years for LC lim - TODO: strcutural period selection
#         window <- 30
#         LC_data <- lt_data %>% 
#                 filter(Date > (min(gap_after)-window)) %>% 
#                 split(list(.$Date,.$Sex)) %>% 
#                 map(remove_zero_rates) %>% # remove zeros proccedure
#                 do.call(rbind,.)
#         
#         # apply LC
#         gap_after_data <- lc(input = LC_data, dates_out = gap_after)%>% 
#                 split(list(.$Date, .$Sex)) %>% 
#                 lapply(function(X){
#                         LT <- lt_single_mx(nMx = X$nMx,
#                                            Age = X$Age,
#                                            Sex = unique(X$Sex),
#                                            OAnew = 100)
#                         LT$Sex <- unique(X$Sex)
#                         LT$Date <- as.numeric(unique(X$Date))
#                         LT$Type <- "LC"
#                         LT
#                 }) %>% 
#                 do.call(rbind,.)
# }
# 
# # diagnostics ------------------------------------------------------------
# final_data <- bind_rows(lt_data %>% filter(Type == "main"),
#                         gap_before_data,
#                         gap_before_data_fit_e0,
#                         gap_after_data)
# plot_age_time(final_data %>% filter(Type != "LC-Lim-fit_e0"))
# plot_ex_time(final_data)
# 
# # compare wpp
