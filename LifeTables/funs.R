# author: IW
# set of functions used in `fill_gaps.R` and `fill_gaps_lt.R`.

# read short notes
download_db_reading_short_notes <- function(myLocID, myIndicators){
        
        serverURL = "https://popdiv.dfs.un.org/DemoData/api/"
        shortnotes_URL <- "https://popdiv.dfs.un.org/peps/eagle/api/notes/get/longcatalogs/" 
        options(unpd_server = serverURL)

        ## query Shortnotes
        shortnotes <- fromJSON(paste0(shortnotes_URL, WPP_RevID, "/", myLocID, "/OverallMortality"), flatten=TRUE)
        
        shortnotes1 <- data.table(shortnotes$DataCatalogsWithSelections)
        shortnotes2 <- data.table(shortnotes$IndicatorMetadata)
        shortnotes <- merge(shortnotes1[,.(DataCatalogID, IndicatorInternalName, SelectionValue)], shortnotes2[, .(IndicatorInternalName, DataProcessTypeIDs, DataStatusIDs, DataTypeIDs, IndicatorIDs)], by="IndicatorInternalName", all.x=TRUE, all.y=FALSE)
        shortnotes_used <- shortnotes[SelectionValue %in% c("Used", "Considered", "NotConsidered")]
        shortnotes_used[, LocID := myLocID]
        
        shortnotes_adult <- fromJSON(paste0(shortnotes_URL, WPP_RevID, "/", myLocID, "/AdultMortality"), flatten=TRUE)
        shortnotes_adult1 <- data.table(shortnotes_adult$DataCatalogsWithSelections)
        shortnotes_adult2 <- data.table(shortnotes_adult$IndicatorMetadata)
        shortnotes_adult <- merge(shortnotes_adult1[,.(DataCatalogID, IndicatorInternalName, SelectionValue)], shortnotes_adult2[, .(IndicatorInternalName, DataProcessTypeIDs, DataStatusIDs, DataTypeIDs, IndicatorIDs)], by="IndicatorInternalName", all.x=TRUE, all.y=FALSE)
        shortnotes_adult_used <- shortnotes_adult[SelectionValue %in% c("Used", "Considered", "NotConsidered")]
        shortnotes_adult_used[, LocID := myLocID]
        shortnotes_used <- unique(rbind(shortnotes_used, shortnotes_adult_used))
        shortnotes_used <- shortnotes_used[SelectionValue %in% c("Used")]
        
        ## "melt" as long format and split DataTypeIDs into rows for easier merging with myDT
        if (nrow(shortnotes_used)>0) {
                shortnotes_used <- shortnotes_used[, .(DataTypeIDs=paste(DataTypeIDs, collapse=",")), by=c("LocID", "DataCatalogID", "SelectionValue")][, c(DataTypeIDs=strsplit(DataTypeIDs, ",")), by=c("LocID", "DataCatalogID", "SelectionValue")]
                shortnotes_used[, DataTypeIDs := as.numeric(DataTypeIDs)]
        }
        shortnotes_used <- unique(shortnotes_used)
        setnames(shortnotes_used, "DataTypeIDs", "DataTypeID")

        DataCatalog <- data.table(get_datacatalog(locIds = myLocID, isSubnational = FALSE))
        DataType <- data.table(get_datatypes())
        shortnotes_used <- merge(shortnotes_used, DataCatalog[, .(LocID, DataCatalogID, DataProcessTypeID, DataProcessTypeShortName, DataProcessID, DataProcessShortName, ShortName)], by=c("LocID", "DataCatalogID"), all.x=TRUE, all.y=FALSE)
        shortnotes_used <- merge(shortnotes_used, DataType[, .(DataTypeID=PK_DataTypeID, DataTypeGroupID, DataTypeShortName=ShortName, DataTypeSortOrder=SortOrder, DataTypeGroupID2, DataTypeGroupName2)], by="DataTypeID", all.x=TRUE, all.y=FALSE)
        
        DT <- data.table(get_recorddataadditional(
                dataProcessIds = unique(shortnotes_used$DataProcessID),
                dataTypeIds =  unique(shortnotes_used$DataTypeID),
                startYear = 1940,
                endYear = 2021,
                indicatorIds = myIndicators, 
                locIds = myLocID,
                locAreaTypeIds = 2,
                subGroupIds = 2,
                isComplete = 0
        ))
        DT <- merge(DT, shortnotes_used[,.(LocID, DataCatalogID, DataTypeID, SelectionValue)], 
                    by=c("LocID", "DataCatalogID", "DataTypeID"))
        return(DT)
}

# old age adj plan B: ratio method
old_age_adj <- function(final_data, dates_adj, min_age_adj){
        dates_adj <- floor(dates_adj) + .5
        dates_ref <- pmin(max(dates_adj) + 1:5, 2020.5)
        ages_adj  <- min_age_adj:100
        ages_ref  <- (min(ages_adj)-10):(min(ages_adj)-1)
        # females
        Mf <- final_data %>%
                filter(Sex=="f") %>% 
                select(Date,nMx,Age) %>%
                arrange(Date,Age) %>% 
                pivot_wider(names_from = Date, values_from = nMx)   %>% 
                select(-Age) %>% as.matrix()
        rates_obs_ref <- colMeans(Mf[ages_ref+1, as.character(dates_adj)])
        rr <- rowMeans(Mf[ages_adj+1, as.character(dates_ref)] / 
                               matrix(colMeans(Mf[ages_ref+1, as.character(dates_ref)]),
                                      nrow = length(ages_adj), ncol = 5, byrow = T)
        )
        Mf[ages_adj+1, as.character(dates_adj)] <- sapply(rr, function(x) x * rates_obs_ref) %>% t()
        old_adj_f <- Mf %>% 
                as.data.frame() %>% 
                mutate(Age = 0:100) %>% 
                pivot_longer(cols=-Age, names_to="Date",values_to="nMx") %>% 
                mutate(Sex = "f")
        # for males
        Mm <- final_data %>%
                filter(Sex=="m") %>% 
                select(Date,nMx,Age) %>%
                arrange(Date,Age) %>% 
                pivot_wider(names_from = Date, values_from = nMx)   %>% 
                select(-Age) %>% as.matrix()
        rates_obs_ref <- colMeans(Mm[ages_ref+1, as.character(dates_adj)])
        rr <- rowMeans(Mm[ages_adj+1, as.character(dates_ref)] / 
                               matrix(colMeans(Mm[ages_ref+1, as.character(dates_ref)]),
                                      length(ages_adj), ncol = 5, byrow = T)
        )
        Mm[ages_adj+1, as.character(dates_adj)] <- sapply(rr, function(x) x * rates_obs_ref) %>% t()
        old_adj_m <- Mm %>% 
                as.data.frame() %>% 
                mutate(Age = 0:100) %>% 
                pivot_longer(cols=-Age, names_to="Date",values_to="nMx") %>% 
                mutate(Sex = "m")
        # return
        out <- bind_rows(old_adj_f,old_adj_m) %>% 
                mutate(Date = as.numeric(Date)) %>% 
                left_join(final_data %>% select(-nMx), by = c("Date","Age","Sex")) %>% 
                split(list(.$Date, .$Sex), drop = T) %>% 
                lapply(function(X){
                        LT <- lt_single_mx(X$nMx, X$Age, Sex = unique(X$Sex), OAG = T, extrapFrom = OAnew)
                        LT$Date <- as.numeric(unique(X$Date))
                        LT$Source <- unique(X$Source)
                        LT$DataProcess <- unique(X$DataProcess)
                        LT$Sex  <- unique(X$Sex)
                        return(LT)}) %>% 
                bind_rows()
        return(out)
}

# constrained extension
lt_extrap_constrained <- function(nMx, Age, 
                                  Sex = "m",
                                  extrapFrom = NULL, # take last age 
                                  method_ex = "classical", 
                                  extrapLaw = "Gompertz",
                                  OAnew = 100,
                                  Single = F, # for output only
                                  k = 1, # how many previous ages for extrap
                                  alpha = 1.4, # for non classical e(OAG) computation 
                                  beta = .095, # for non classical e(OAG) computation
                                  r = .01, # for non classical e(OAG) computation
                                  x_hat = 90 # for non classical e(OAG) computation
){
        
        # control check
        stopifnot(is_abridged(Age))
        if(is.null(extrapFrom)) extrapFrom <- max(Age)
        
        # open age rate
        Ma <- last(nMx)
        
        # life expectancy computation
        if(method_ex == "classical"){
                ex_obj <- 1/Ma
        }
        if(method_ex == "H-C"){
                ex_obj <- 1/Ma*exp(-beta*r*Ma^(-alpha))
        }
        if(method_ex == "Mitra"){
                ex_obj <- 1/Ma*exp(-r*(1/Ma-(1+r*1/Ma)*(x_hat-extrapFrom)))
        }
        
        # choose b for fitting ex_ob
        Age_extrap <- c(0,1,seq(5,OAnew,5))
        nMx_extrap <- c(nMx,rep(NA,length(Age_extrap)-length(Age)))
        b_optim <- optimise(fo_extrap, interval=c(.001,1), 
                            Age=Age_extrap, nMx = nMx_extrap, Sex = Sex,
                            extrapFrom = extrapFrom, ex_obj = ex_obj, 
                            extrapLaw = extrapLaw,
                            k = k)$minimum
        
        # set extrap rates
        nMx_prev <- nMx_extrap[Age_extrap<extrapFrom]
        nMx_extrap <- law_extrap(nMx = nMx_extrap, Age = Age_extrap,
                                 extrapFrom = extrapFrom, 
                                 b = b_optim, extrapLaw = extrapLaw,
                                 k = k)
        nMx_hat <- c(nMx_prev, nMx_extrap)
        
        # set the final lt
        lt_out <- lt_ambiguous(nMx_or_nqx_or_lx = nMx_hat, type = "m", Sex = Sex,
                               Age = Age_extrap, OAnew=100, Single = Single)
        
        return(lt_out)
} 

# function to minimize
fo_extrap <- function(b, Age, Sex = Sex, extrapFrom, ex_obj, nMx, extrapLaw, k){
        nMx_extrap <- law_extrap(nMx = nMx, Age = Age, extrapFrom = extrapFrom, b = b, extrapLaw = extrapLaw, k=k)
        nMx_prev <- nMx[Age<extrapFrom]
        nMx_hat <- c(nMx_prev, nMx_extrap)
        ex_hat <- lt_ambiguous(nMx_hat, "m", Age, Sex = Sex, OAnew=100, Single=F) %>% filter(Age==extrapFrom) %>% pull(ex)
        # quadratic relative diff
        quad_dif <- (ex_hat/ex_obj-1)^2
        return(quad_dif)
}

# given parameter "b", apply extrapolation law from some age "extrapFrom". 
# using the mean of the log of "k" 5age-rate-observations before OAG
law_extrap <- function(nMx, Age, extrapFrom, b, extrapLaw = "Gompertz", k=3){
        # where is the starting age for extrapolate
        id <- which(Age==extrapFrom)
        # index of observations to average
        id_mean <- seq(id - 1,length.out = k, by=-1)
        # apply some law
        if(extrapLaw == "Gompertz"){
                C_mean <- exp(mean(log(nMx[id_mean])))
                nMx_hat <- C_mean * exp(b*(Age-extrapFrom+5))
        }
        if(extrapLaw == "Kannisto"){
                C_mean <- exp(mean(log(nMx[id_mean])))/(1-exp(mean(log(nMx[id_mean]))))
                nMx_hat <- C_mean * exp(b*(Age-extrapFrom+5))/(1 + C_mean * exp(b*(Age-extrapFrom+5)))
        }
        # return only ages that matters
        return(nMx_hat[Age>=extrapFrom])
}


# fill interval gaps
fill_intervals_gaps <- function(intervals_gaps, main_data_time, 
                                lt_data, first_year, last_year,
                                Empirical_LT_LeeCarter_time_window_fit_Limited_LC,
                                Empirical_LT_LeeCarter_time_window_fit_LC,
                                Empirical_LT_LeeCarter_non_divergent,
                                Empirical_LT_LeeCarter_jump_off,
                                Empirical_LT_LeeCarter_fit_e0,
                                Single){
        
        # take structure from lt_data
        gap_middle_data <- gap_before_data <- gap_after_data <- lt_data %>% filter(Age==-1)
        
        # decide for different kind of gaps
        for(gap in intervals_gaps){
                # gap in the left or all
                if((first_year+.5) %in% gap | all(c(first_year,last_year)+.5) %in% gap){
                        # if we have some lonely data point to pivot 
                        if(any(unique(lt_data$Date)<max(gap))){
                                window <- min(lt_data$Date[lt_data$Type=="main"])+Empirical_LT_LeeCarter_time_window_fit_Limited_LC
                                LClim_data <- lt_data %>% 
                                        filter(Date >= 1940, Date <= window) %>% 
                                        arrange(Date,Age) 
                                # if fit e0 for kt parameter
                                if(!Empirical_LT_LeeCarter_fit_e0){
                                        gap_before_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap),
                                                                         Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat
                                }else{
                                        # takes last observed and interval not main source points
                                        dates_e0 <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0) %>% 
                                                select(Date) %>% unique() %>% pull()
                                        e0_Males <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0, Sex == "m") %>% 
                                                select(ex) %>% pull()
                                        e0_Females <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0, Sex == "f") %>% 
                                                select(ex) %>% pull()
                                        gap_before_data <- try(interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                         dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                         Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat)
                                        if("try-error" %in% class(gap_before_data)){
                                                gap_before_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                                     dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                                     Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent,
                                                                                     extrapLaw = "Makeham")$lt_hat
                                        }
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
                if(2020.5 %in% gap & !all((c(first_year,last_year)+.5) %in% gap)){
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
                                                                         Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat
                                }else{
                                        dates_e0 <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0) %>% 
                                                select(Date) %>% unique() %>% pull()
                                        e0_Males <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0, Sex=="m") %>% 
                                                select(ex) %>% pull()
                                        e0_Females <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0, Sex=="f") %>% 
                                                select(ex) %>% pull()
                                        gap_after_data <- try(interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                        dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                        Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat %>% 
                                                                        mutate(Type = "LC-Lim", Source = "LC-Lim"))
                                        if("try-error" %in% class(gap_after_data)){
                                                gap_after_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                                    dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                                    Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent,
                                                                                     extrapLaw = "Makeham")$lt_hat
                                        }
                                }
                        }
                        if(length(unique(LC_data$Date)) %in% 3:5){
                                LClim_data <- LC_data
                                if(!Empirical_LT_LeeCarter_fit_e0){
                                        gap_before_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap),
                                                                         Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat
                                }else{
                                        # takes last observed and interval not main source points
                                        dates_e0 <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0) %>% 
                                                select(Date) %>% unique() %>% pull()
                                        e0_Males <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0, Sex == "m") %>% 
                                                select(ex) %>% pull()
                                        e0_Females <- lt_data %>% filter(Date<=min(LClim_data$Date[LClim_data$Type=="main"]), Age==0, Sex == "f") %>% 
                                                select(ex) %>% pull()
                                        gap_after_data <- try(interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                        dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                        Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat %>% 
                                                mutate(Type = "LC-Lim", Source = "LC-Lim"))
                                        if("try-error" %in% class(gap_after_data)){
                                                gap_after_data <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                        dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                        Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent,
                                                                        extrapLaw = "Makeham")$lt_hat %>% 
                                                                      mutate(Type = "LC-Lim", Source = "LC-Lim")
                                        }
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
                                                                 Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat
                        }else{
                                dates_e0 <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0) %>% 
                                        select(Date) %>% unique() %>% pull()
                                e0_Males <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0, Sex=="m") %>% 
                                        select(ex) %>% pull()
                                e0_Females <- LClim_data %>% filter(Type!="main" | Date %in% c(min(gap)-1,max(gap)+1), Age==0, Sex=="f") %>% 
                                        select(ex) %>% pull()
                                gap_middle_data_i <- try(interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                   dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                   Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent)$lt_hat)
                                if("try-error" %in% class(gap_middle_data_i)){
                                gap_middle_data_i <- interp_lc_lim(input = LClim_data, dates_out = as.numeric(gap), 
                                                                       dates_e0 = dates_e0, e0_Males = e0_Males, e0_Females = e0_Females,
                                                                       Single = Single, prev_divergence = Empirical_LT_LeeCarter_non_divergent,
                                                                       extrapLaw = "Makeham")$lt_hat
                                }
                        } 
                        gap_middle_data <- bind_rows(gap_middle_data,gap_middle_data_i)
                }
        }

        # final data recovering inputs from LC-Lim
        final_data <- bind_rows(lt_data %>% 
                                        filter(Type == ifelse(is.null(main_data_time),"add","main"), 
                                               Date>=first_year),
                                gap_before_data,
                                gap_middle_data %>% mutate(Type = "LC-Lim", Source = "LC-Lim"),
                                gap_after_data) %>% 
                left_join(lt_data %>% filter(Type=="add") %>% 
                                  distinct(Date,Source) %>% rename(Source2=Source),
                          by = "Date") %>%
                mutate(Source = ifelse(!is.na(Source2),Source2,Source)) %>% 
                select(-Source2, -Type) %>% 
                filter(!Source %in% c("WPP19"))
        # plot_ex_time(final_data,country = "pepe")
        
        return(final_data)
}

# get id, shor name or compelte name of countries. `fertestr` is used
get_ID_Name <- function(x){
        wpp_locs <- fertestr::locs_avail()
        out <- wpp_locs %>% 
                filter(x==location_code | x==location_name | toupper(x)==location_code_iso3) %>% 
                select(PK_LocID=location_code, 
                       Name=location_name,
                       Code_iso = location_code_iso3)
        stopifnot(nrow(out)==1)
        return(out)
}

# add wpp19 data for 1950-1955 in case it is needed. Warning: not all countries are there.
add_wpp19_1950data <- function(country, country_data){
        wpp19_lt <- DemoToolsData::WPP2019_lt %>%
                filter(LocID == country, Year == 1953, Sex!="b") %>% 
                mutate(TimeLabel = "1950-1955", TimeMid = Year, DataSourceShortName = "WPP19") %>%
                pivot_longer(cols = mx:lx, names_to = "IndicatorName", values_to = "DataValue") %>% 
                mutate(IndicatorName = ifelse(IndicatorName=="lx", "l(x) - abridged", "m(x,n) - abridged"),
                       DataReliabilitySort = NA,
                       DataTypeSort=1000) %>% 
                rename(SexName=Sex) %>% 
                select(-LocID, -Year)
        out <- country_data %>% bind_rows(wpp19_lt)
        return(out)
}

# ask if an indicator is "m" or "l" type
is_m_or_l <- function(x){
        ifelse(str_detect(x, "m\\(x"),"m",
                   ifelse(str_detect(x, "l\\(x"),"l",NA))
}

# check data validation: can be labelled as abridged or complete?
is_data_valid <- function(country_data, pop){
        validation <- country_data %>%
                split(list(.$IndicatorName, .$DataSourceShortName, 
                           .$SexName, .$TimeLabel, .$DataTypeSort), drop = T) %>% 
                lapply(function(X){
                        # X <- country_data %>% filter(TimeMid==2000.5, DataSourceShortName=="WHR 2002", SexName=="f")
                        validated <- coherence <- "not LT"
                        abr_ages <- c(0,1,seq(5,60,5)) 
                        # is a lifetable first, then wich one
                        if(str_detect(X$IndicatorName[1],"l\\(x|m\\(x")){
                                X <- X %>% filter(AgeStart<=100)
                                Age <- X$AgeStart
                                AgeInt <- X$AgeSpan
                                AgeInt[AgeInt<1] <- NA
                                pop_total <- pop %>% 
                                                filter(Year==unique(X$TimeMid)) %>% 
                                                summarise(sum(m+f)) %>% pull() * 1000
                                # is coherent
                                coherence <- suppressMessages(is_age_coherent(Age, AgeInt))
                                # is single
                                single_ok <- is_single(Age) & all(0:60 %in% Age)
                                # is abridg
                                abridg_ok <- is_abridged(Age[!Age %in% 2:4]) & all(abr_ages %in% Age) # to capture 0,1,2,4
                                # neg or na
                                neg_or_NA <- any(X$DataValue<0 | is.na(X$DataValue))
                                # 0 conditions only in larger populations
                                irregular <- FALSE
                                if(pop_total>1e6){
                                        if(single_ok){
                                                irregular <- sum(diff(X$DataValue)==0) > round(length(X$DataValue)/3,0) # more than 1/3 of ages without risk                        
                                        }else if(abridg_ok){
                                                irregular <- sum(diff(X$DataValue)==0) > 1 # accept 1 risk nule on lx
                                                irregular <- sum(X$DataValue==0) > 1 # accept only 1 risk nule on m(x,n)
                                        }else{
                                                irregular <- TRUE
                                        }        
                                }
                                # is validated
                                validated <- ifelse(single_ok, "complete", 
                                                   ifelse(abridg_ok,"abridged", 
                                                          ifelse(neg_or_NA | irregular, "zeros", 
                                                                ifelse(!single_ok & !abridg_ok, "error"))))
                        }
                        validation_i <- X %>% mutate(validation = validated, coherence = coherence)
                        return(validation_i)
                }
                ) %>% 
                do.call(rbind,.) %>% 
                remove_rownames(.) %>% 
                distinct()
        return(validation)
}

# impute data cases: 
        # fill/smooth zeroes ages, 
        # split an abridged lt with initial group 0-5
        # group into 1-5 in case and abr lt is split by simple age on those ages 
impute_data <- function(data, pop, epsilon = NULL){

        # remove zeros
        if(is.null(epsilon)){
                epsilon_age <- data %>% 
                        filter(is_m_or_l(IndicatorName)=="m", DataValue>0) %>% 
                        group_by(AgeStart, SexName) %>% 
                        summarise(epsilon= min(min(DataValue)/2, 1e-6))
                # epsilon <- data %>% 
                #         filter(is_m_or_l(IndicatorName)=="m", DataValue>0) %>% 
                #         summarise(min(DataValue)/2) %>% pull()
                # epsilon <- min(epsilon, 1e-6)
        }
        data <- data %>% 
                split(list(.$DataSourceShortName,.$TimeLabel,.$IndicatorName,
                           .$DataTypeSort,.$SexName), drop=T) %>% 
                  lapply(function(X){
                        # if(unique(X$TimeMid)==1951.5 & unique(X$SexName=="f")){browser()}
                        # X <- data %>% filter(TimeMid==2010.5,SexName=="f", IndicatorName=="m(x,n) - abridged", DataSourceShortName=="VR(WPP)")
                        out <- X
                        if(!unique(X$validation) %in% c("not LT","error")){
                                if(is_m_or_l(unique(X$IndicatorName))=="m" & any(X$DataValue<=0)){
                                        # gives
                                        out <- out %>% left_join(epsilon_age, by=c("AgeStart","SexName")) %>% 
                                                        mutate(has_zero = ifelse(DataValue<=0,1,0),
                                                        DataValue = ifelse(has_zero==1, epsilon, DataValue),
                                                        # DataAverage = slide(DataValue, mean, .before = 1, .after = 1) %>% unlist(),
                                                        # DataValue = ifelse(has_zero==1,DataAverage,DataValue),
                                                        validation = "rm_0s") %>% 
                                                select(-has_zero, -epsilon
                                                       # ,-DataAverage
                                                       )
                                } 
                                if(is_m_or_l(unique(X$IndicatorName))=="l" & any(diff(X$DataValue)>=0)){
                                        # browser()
                                        input_no_zeros <- X %>% filter(c(-1,diff(X$DataValue))<0) # remove no risk at all
                                        Age_output <- X$AgeStart
                                        lx <- data.frame(Age = Age_output, 
                                                         lx = splinefun(x = input_no_zeros$AgeStart, 
                                                                        y = input_no_zeros$DataValue, 
                                                                        method = "monoH.FC")(Age_output))
                                        out$DataValue <- lx$lx
                                        out$validation = "rm_0s"
                                } 
                        }
                        out
                 }) %>% 
                bind_rows()
        
        
        # split in case 0-5 is given
        # an id just to track filter
        data$ID <- 1:nrow(data)
        # detect 0-5 cases on mx (not lx)
        data_to_split <- data %>% 
                        filter(is_m_or_l(IndicatorName)=="m", AgeStart==0, AgeSpan==5) %>%
                        mutate(to_split = 1) %>% 
                        select(DataSourceShortName,TimeLabel,IndicatorName,SexName,DataTypeSort,to_split) %>% 
                        inner_join(data,by=c("DataSourceShortName","TimeLabel","IndicatorName","SexName","DataTypeSort")) %>% 
                        filter(to_split==1) %>% 
                        distinct()
                
        if(nrow(data_to_split)>0){
                # browser()
                data_imputed <- data_to_split %>% 
                                split(list(.$DataSourceShortName, .$TimeLabel, .$SexName, .$DataTypeSort), drop=T) %>% 
                                map_df(.f = function(X) {split_abr_0_5(X, pop = pop)})
                data_output <- rbind(data_imputed %>% 
                                                mutate(validation = "splitted") %>% 
                                                select(-to_split),
                                        data %>% filter(!ID %in% data_imputed$ID))
        }else{
                data <- data 
        }
        
        # abr lt with ages 0:5
        # an id just to track filter
        data$ID <- 1:nrow(data)
        # detect 0-5 cases on mx (not lx)
        data_to_agr <- data %>% 
                filter(is_m_or_l(IndicatorName)=="m",
                       validation == "abridged") %>%
                group_by(DataSourceShortName,TimeLabel,IndicatorName,SexName,DataTypeSort) %>% 
                mutate(to_agr = ifelse(2 %in% AgeStart & 3 %in% AgeStart & 4 %in% AgeStart, 1, 0)) %>% 
                ungroup() %>% 
                distinct(DataSourceShortName,TimeLabel,IndicatorName,SexName,DataTypeSort,to_agr) %>% 
                inner_join(data,by=c("DataSourceShortName","TimeLabel","IndicatorName","SexName","DataTypeSort")) %>% 
                filter(to_agr==1) %>% 
                distinct()
        
        if(nrow(data_to_agr)>0){
                # browser()
                data_imputed <- data_to_agr %>% 
                        split(list(.$DataSourceShortName, .$TimeLabel, .$SexName, .$DataTypeSort), drop=T) %>% 
                        map_df(.f = function(X) {agr_1_5(X)})
                data_output <- rbind(data_imputed %>% 
                                             mutate(validation = "grouped") %>% 
                                             filter(!AgeStart %in% 2:4) %>% 
                                             select(-to_agr),
                                     data %>% filter(!ID %in% data_imputed$ID))
        }else{
                data_output <- data 
        }
        
        # set as error if has 2 or 3 or 4 only. Will use lx instead
        data_output <- data_output %>%
                group_by(DataSourceShortName,TimeLabel,IndicatorName,SexName,DataTypeSort) %>% 
                mutate(validation = ifelse(is_m_or_l(IndicatorName)=="m" &
                                           validation=="abridged" & 
                                           validation!="grouped" &
                                           any(2:4 %in% AgeStart),"error", validation)) %>% 
                ungroup()
        
        
        data_output <- data_output %>% select(-ID) %>% 
                split(list(.$TimeMid, .$TimeLabel, .$SexName, .$DataTypeSort, .$IndicatorName), drop = T) %>% 
                lapply(function(X){
                        Indicator_type = ifelse(str_detect(unique(X$IndicatorName),"m\\(x,n"),"mx",
                                                ifelse(str_detect(unique(X$IndicatorName),"l\\(x"),"lx",NA))
                        Y <- X
                        # just keep abr ages in case
                        if(Indicator_type=="lx" & unique(X$validation) %in% c("grouped","abridged")){
                                abr_ages <- c(0,1,seq(5,max(X$AgeStart),5))
                                Y <- X %>% filter(AgeStart %in% abr_ages)
                        }
                        # avoid some LT with l(OAG)==0
                        if(Indicator_type=="lx" & last(X$DataValue)==0){
                                Y <- X %>% filter(!AgeStart %in% last(AgeStart))
                        }
                        # avoid super rates that crush DemoTools extrap
                        if(Indicator_type=="mx"){
                                Age_m_bigger_1 <- min(X$AgeStart[X$DataValue>1])
                                Y <- X %>% filter(AgeStart<Age_m_bigger_1)
                        }
                        return(Y)
                }) %>% bind_rows()
        
        # output
        return(data_output)
}

# group first ages of an abr lt in case it has 0:4 ages
agr_1_5 <- function(X){
        # X=data_to_agr %>% filter(TimeLabel==1950,SexName=="f")
        new_nMx <- lt_ambiguous(nMx_or_nqx_or_lx = X$DataValue, type = "m", Age = 0:(nrow(X)-1), 
                                Sex = unique(X$SexName), Single = F) %>% 
                filter(Age %in% 0:1) %>% pull(nMx)
        X$DataValue[X$AgeStart==0] = new_nMx[1]
        X$DataValue[X$AgeStart==1] = new_nMx[2]
        X$AgeSpan[X$AgeStart==1] = 4
        out <-X
        return(out)
}

# split first group in case is 0-5
split_abr_0_5 <- function(X, pop){
        # x <- data_to_split %>% slice(1)
        # country_LocID = country_full$PK_LocID
        # browser()
        x <- X %>% filter(AgeStart==0)
        pop <- pop %>% filter(Year==floor(unique(x$TimeMid))+.5, AgeStart<5) %>% 
                summarise(pop0_m = sum(m[AgeStart==0]),
                          pop0_f = sum(f[AgeStart==0]),
                          pop1_4_m = sum(m[AgeStart %in% 1:4]),
                          pop1_4_f = sum(f[AgeStart %in% 1:4]))
        if(is_m_or_l(x$IndicatorName)=="m"){
        if(x$SexName=="m"){
                pop_s <- data.frame(pop0_s = pop$pop0_m, pop1_4_s = pop$pop1_4_m, 
                         pop0_4_s = sum(pop$pop0_m, pop$pop1_4_m))         
        }else(
                pop_s <- data.frame(pop0_s = pop$pop0_f, pop1_4_s = pop$pop1_4_f, 
                         pop0_4_s = sum(pop$pop0_f, pop$pop1_4_f))
        )
                
                M0_5 <- x$DataValue
                M0_1   <- lt_rule_4m0_m0(M04 = M0_5, Sex = x$SexName)
                M1_4 <- (M0_5 * pop_s$pop0_4_s - M0_1 * pop_s$pop0_s)/pop_s$pop1_4_s
        } 
        output <- rbind(x,x)
        output$AgeSpan[1] <- 1
        output$DataValue[1] <- M0_1
        output$AgeStart[2] <- 1
        output$AgeSpan[2] <- 4
        output$DataValue[1] <- M1_4
        out <- bind_rows(output, X[X$AgeStart!=0,])
        return(out)
}

# select data for each time
select_data <- function(country_data){
        
        # herarchy structure
        sources_available <- tibble(DataSourceShortName=unique(country_data$DataSourceShortName))
        source_herarchy <- tibble(DataSourceShortName = c("HMD","EuroStat","VR(WPP)","WHO DB",
                                                         "HLD 2020","DYB","GBD 2016","WPP19"),
                                  Index = 1:8)
        source_herarchy_available <- left_join(sources_available,source_herarchy, by="DataSourceShortName") %>% 
                arrange(Index, desc(DataSourceShortName))
        source_herarchy_available$Index <- 1:nrow(source_herarchy_available)
        
        # select by floor dates
        country_data$TimeMid_floor <- floor(country_data$TimeMid) 
        time_data <- sort(unique(country_data$TimeMid_floor))
        
        # detect lonely points
        lonely_points <- country_data %>% 
                count(DataSourceShortName, TimeMid_floor) %>% 
                pivot_wider(names_from=DataSourceShortName, values_from=n) %>% 
                right_join(tibble(TimeMid_floor = 1940:2020), by = "TimeMid_floor") %>% 
                arrange(TimeMid_floor) %>% 
                mutate_at(vars(-TimeMid_floor),
                          ~ifelse(!is.na(.)&is.na(lag(.))&is.na(lead(.)),1,0)) %>% 
                pivot_longer(!TimeMid_floor, names_to = "DataSourceShortName", values_to = "Lonely")
        
        # selection: turno to map when it is final
        for(year in time_data){
                # if(year == 2017)browser()
                selection_y <- country_data %>%
                        filter(TimeMid_floor == year, validation!="not LT") %>%
                        group_by(DataSourceShortName, TimeMid_floor, TimeMid, TimeLabel, 
                                 IndicatorName, nrank3, DataTypeSort) %>% 
                        summarise(complete = is_single(unique(AgeStart)),
                                  OAG = max(AgeStart)) %>% 
                        mutate(is_avg_years = ifelse(substr(TimeLabel,6,9)=="",0,
                                                     as.integer(substr(TimeLabel,6,9))-
                                                     as.integer(substr(TimeLabel,1,4)))) %>% 
                        left_join(source_herarchy_available,by = "DataSourceShortName") %>%
                        left_join(lonely_points,by = c("DataSourceShortName","TimeMid_floor")) %>%
                        ungroup() %>% 
                        arrange(Lonely, # not lonely points
                                Index, # herarchy
                                nrank3, # an country-specific or LAMBDA should be selected, even not included in Index
                                DataTypeSort, # this take care of HLD options
                                desc(complete), # firs complete
                                desc(IndicatorName), # first mx
                                is_avg_years) # first not avg years adta
                best_y <- selection_y %>% slice(1)
                option_y <- selection_y %>% 
                        distinct(DataSourceShortName,TimeMid_floor) %>% 
                        slice(1:2)
                if(year == time_data[1]){
                        selection <- selection_y
                        best <- best_y
                        option <- option_y
                }else{
                        selection <- bind_rows(selection_y, selection)
                        best <- bind_rows(best_y, best)
                        option <- bind_rows(option_y, option)
                }
        }
        
        # decide between sources in casethere is some overlapping years
        desoverl <- desoverlap(option, country_data, source_herarchy_available)
        
        # final selection
        final <- selection %>% 
                left_join(desoverl %>% select(TimeMid_floor,dsF), by=c("TimeMid_floor")) %>% 
                filter(is.na(dsF)|DataSourceShortName==dsF) %>%
                group_by(DataSourceShortName, TimeMid) %>% 
                arrange(TimeMid,
                        desc(complete), # firs complete
                        desc(IndicatorName)) %>% 
                slice(1) %>% 
                distinct() %>% 
                mutate(DataSourceShortName=dsF) %>% select(-dsF) %>%
                ungroup() %>% 
                arrange(TimeMid)
        
        # remove lonelys just next to serie in case are not all disperse HLD
        if(!all(final$DataSourceShortName=="HLD 2020")){
                final <- final %>% 
                        mutate(Dif = TimeMid-lead(TimeMid),
                               Dif = ifelse(is.na(Dif),-10,Dif)) %>% 
                        filter(!(Lonely==1 & Dif==-1))
        }
        
        # return
        return(final)
}

# decide on overlapping periods
desoverlap <- function(option, country_data, source_herarchy, treshold = .02, ...){
        
        # detect overlaps
        overl <- option %>%
                group_by(TimeMid_floor) %>% mutate(id = row_number()) %>% 
                pivot_wider(names_from=id,values_from=DataSourceShortName)
        # some countries have only one source for each available year (Haiti)
        if(ncol(overl)==2){
                overl$`2` <- NA
        }
        overl <- overl %>% rename(ds1=2, ds2=3) %>% 
                arrange(TimeMid_floor) %>% 
                mutate(overlap = ifelse(!is.na(ds1)&!is.na(ds2),1,0)
                       # dif = ifelse(ds1!=lead(ds1) & ds2!=lead(ds2),1,0)
                ) %>% 
                mutate(ds2 = ifelse(is.na(ds2),"No",ds2)) %>% 
                as.data.frame()
        
        # separate groups (not proud of this code)
        group = 1
        for(i in 2:nrow(overl)){
                # i = 2
                if(overl$ds1[i]==overl$ds1[i-1] & overl$ds2[i]==overl$ds2[i-1] & 
                   (overl$TimeMid_floor[i]-overl$TimeMid_floor[i-1])==1){
                        group[i] = group[i-1] 
                }else{
                        group[i] = group[i-1]+1 
                }
        }
        overl$groups <- group
        
        # set final ds for each
        desoverl <- overl %>% 
                split(list(.$ds1,.$ds2, .$groups), drop=T) %>% 
                lapply(function(X){
                        Xds1 <- unique(X$ds1)
                        Xds2 <- unique(X$ds2)
                        Date <- X$TimeMid
                        this_sex <- unique(country_data$SexName)
                        # if not overlap
                        if(unique(X$overlap) == 0){
                                out <- X %>% mutate(dsF = ds1)
                        }
                        # ds2 is lonely
                        else if(nrow(X) == 1){
                                out <- X %>% mutate(dsF = ds1)
                        }
                        # ds2 is more bad source
                        else if(source_herarchy$Index[source_herarchy$DataSourceShortName==Xds2] %in% 5:8){
                                out <- X %>% mutate(dsF = ds1)
                        }
                        # ds1 has 10 or more years, we respect that
                        else if(nrow(X)>=10){
                                out <- X %>% mutate(dsF = ds1)
                        }
                        # ds1 has less than 10 years is a bridge btwn ds2, check how fits
                        else{
                                # browser()
                                date_prev <- min(Date)-1
                                date_post <- max(Date)+1
                                ds_prev <- option %>% filter(TimeMid_floor %in% date_prev) %>% slice(1) %>% pull(DataSourceShortName)
                                ds_post <- option %>% filter(TimeMid_floor %in% date_post) %>% slice(1) %>% pull(DataSourceShortName)
                                if(length(ds_prev)>0 & length(ds_post)>0 & all(ds_prev==Xds2,ds_post==Xds2)){
                                        l1_prev <- country_data %>% 
                                                filter(DataSourceShortName == ds_prev,
                                                       TimeMid_floor %in% date_prev, SexName==this_sex, 
                                                       str_detect(IndicatorName,"l\\(x")) %>% 
                                                lt_ambiguous(nMx_or_nqx_or_lx = .$DataValue,
                                                             type = "l", Sex = this_sex,
                                                             Age = .$AgeStart, Single = FALSE)
                                        l1_post <- country_data %>% 
                                                filter(DataSourceShortName == ds_prev,
                                                       TimeMid_floor %in% date_post, SexName==this_sex, 
                                                       str_detect(IndicatorName,"l\\(x"))%>% 
                                                lt_ambiguous(nMx_or_nqx_or_lx = .$DataValue,
                                                             type = "l", Sex = this_sex,
                                                             Age = .$AgeStart, Single = FALSE)
                                        l1_ds1 <- country_data %>% 
                                                filter(DataSourceShortName == Xds1,
                                                       TimeMid_floor %in% Date, SexName==this_sex, 
                                                       str_detect(IndicatorName,"l\\(x")) %>% 
                                                split(list(.$TimeMid)) %>% 
                                                lapply(function(X){
                                                        lt_ambiguous(nMx_or_nqx_or_lx = X$DataValue,
                                                                     type = "l", Sex = this_sex,
                                                                     Age = X$AgeStart, Single = FALSE)         
                                                }) %>% bind_rows(.)
                                        b = (l1_post$ex[1]-l1_prev$ex[1])/(date_post-date_prev)
                                        e0_hat <- l1_prev$ex[1] + (Date-date_prev) * b
                                        e0_ds1 <- l1_ds1 %>% filter(Age==0) %>% arrange(Date) %>% pull(ex)
                                        # plot(c(date_prev,date_post),c(l1_prev$ex[1],l1_post$ex[1]),col=2,
                                        #      ylim=c(l1_prev$ex[1]-5,l1_post$ex[1]+5), ylab="e(0)",xlab="Year")
                                        # lines(Date,e0_hat,t="o", lt=2)
                                        # points(Date,e0_ds1,col=4)
                                        # relative diference
                                        rel_dif_lineal <- (e0_hat-e0_ds1)/e0_hat
                                        # greater than 2%
                                        if(any(abs(rel_dif_lineal)>treshold)){
                                                out <- X %>% mutate(dsF = ds2)
                                        }else{
                                                out <- X %>% mutate(dsF = ds1)
                                        }
                                }else{
                                        out <- X %>% mutate(dsF = ds1)
                                }
                        }
                        return(out)
                }) %>% bind_rows(.) %>% arrange(TimeMid_floor)
        return(desoverl)
}

# remove zero rates: spline on lx on those zero cells
remove_zero_rates <- function(input, Age_output = 0:100, ...){
        # if(input$Date==2019.5){browser()}
        if(isTRUE(any(input$nMx<=0))){
                # browser()
                input_no_zeros <- input %>% filter(c(-1,diff(input$lx))<0) # remove no risk at all
                lx <- data.frame(Age = Age_output, 
                                 lx = splinefun(x = input_no_zeros$Age, 
                                                y = input_no_zeros$lx, 
                                                method = "monoH.FC")(Age_output))
                out <- lt_single_qx(nqx = lt_id_l_q(lx$lx),
                                    Age = lx$Age, OAnew = 100, 
                                    ...)
                out <- bind_cols(out,input[,c("Date","Type","Sex")])
                out$Source <- unique(input$Source) 
                return(out)
        }else{
                return(input)
        }
}

# lee carter function
lc <- function(input, dates_out, jump_off = TRUE, prev_cross_over = FALSE, male_weight = .48,
               LC_fit_e0 = FALSE, params_out = FALSE, ...){
        dates_in <- sort(unique(as.numeric(input$Date)))
        ages <- sort(unique(input$Age))
        params <- lc_params(input, male_weight)
        # fit k
        if(LC_fit_e0){
                e0_Males <- input$ex[input$Age==0 & input$Sex=="m"]
                e0_Females <- input$ex[input$Age==0 & input$Sex=="f"]
                e0_Both <- e0_Males * male_weight + e0_Females * (1-male_weight)
                kt_male_star <- kt_female_star <- Kt_star <- c()
                for (j in seq_along(dates_in)){
                        kt_male_star[j] <- optimize(f = interp_lc_lim_kt_min,
                                                    interval = c(-20, 20),
                                                    ax = params$ax_male,
                                                    bx = params$bx_female,
                                                    age = ages,
                                                    sex = "m",
                                                    e0_target = e0_Males[j],
                                                    ...)$minimum
                        kt_female_star[j] <- optimize(f = interp_lc_lim_kt_min,
                                                      interval = c(-20, 20),
                                                      ax = params$ax_female,
                                                      bx = params$bx_female,
                                                      age = ages,
                                                      sex = "f",
                                                      e0_target = e0_Females[j],
                                                      ...)$minimum
                        Kt_star[j] <- optimize(f = interp_lc_lim_kt_min,
                                                      interval = c(-20, 20),
                                                      ax = params$ax_both,
                                                      bx = params$Bx,
                                                      age = ages,
                                                      e0_target = e0_Both[j],
                                                      sex = "b",
                                                      ...)$minimum
                }
        params$kt_male <- kt_male_star
        params$kt_female <- kt_female_star
        params$Kt <- Kt_star
        }
        
        # prev divergence sex: only common factor model (Li, 2005). Neither convergence nor divergence
        if(!prev_cross_over){
                     M_hat_males   <- lc_forecast(params$ax_male,params$bx_male,params$kt_male,params$M_male, Sex="m",
                                                dates_in, dates_out,jump_off,ages)
                     M_hat_females <- lc_forecast(params$ax_female,params$bx_female,params$kt_female,params$M_female,Sex="f",
                                                dates_in, dates_out,jump_off,ages)
             }
        if(prev_cross_over){
                     M_hat_males   <- lc_forecast(params$ax_male,params$Bx,params$Kt,params$M_male, Sex="m",
                                                dates_in, dates_out,jump_off,ages)
                     M_hat_females <- lc_forecast(params$ax_female,params$Bx,params$Kt,params$M_female, Sex="f",
                                                  dates_in, dates_out,jump_off,ages)
                     # if ACF is choosen, then:
                        # forecast each k_ACF and apply:
                        # M_hat <- exp(ax + Bx %*% t(Kt_ACF_forecast) + Bx %*% t(kt_ACF_forecast))
             }
        M_hat <- rbind(M_hat_males, M_hat_females)
        
        # out
        if(params_out){
                return(list(M_hat = M_hat, params = params))
        }else{
                return(M_hat)
        }
     }

# project or retro-project with LC parameters
lc_forecast <- function(ax, bx, kt, M, Sex, dates_in, dates_out, jump_off, ages){
        # browser()
        kt_diff <- diff(kt)
        summary_kt <- summary(lm(kt_diff ~ 1))
        kt_drift <- summary_kt$coefficients[1,1]
        back_after <- ifelse(dates_out[1]>max(dates_in),1,0)
        h <- dates_out - dates_in[1] + back_after
        kt_forecast <- head(kt, 1) + (h * kt_drift)
        # if there is a jump off
        ndates_in <- ncol(M)
        if(jump_off){
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
        M_hat$Age <- ages
        M_hat <- M_hat %>%
                pivot_longer(cols=-ncol(M_hat), names_to="Date",values_to="nMx")
        M_hat$Sex = Sex
        M_hat
}

# LC parameters estimation
lc_params   <- function(input, male_weight = .48){
                # same bx and kt (for the moment)
                M_male <- input %>%
                                filter(Sex=="m") %>% 
                                select(Date,nMx,Age) %>% 
                                pivot_wider(names_from = Date, values_from = nMx)   %>% 
                                select(-Age) %>% as.matrix()
                M_female <- input %>%
                                filter(Sex=="f") %>% 
                                select(Date,nMx,Age) %>% 
                                pivot_wider(names_from = Date, values_from = nMx)   %>% 
                                select(-Age) %>% as.matrix()
                ndates_in <- ncol(M_male)
                
                # males
                ax_male  <- rowSums(log(M_male))/ndates_in
                M_svd_male      <- svd(log(M_male)-ax_male)
                bx_male         <- M_svd_male$u[, 1]/sum(M_svd_male$u[, 1])
                # consistency for b(x) negatives
                bx_male[bx_male<0] <- 0
                bx_male         <- bx_male/sum(bx_male)
                kt_male         <- M_svd_male$d[1] * M_svd_male$v[, 1] * sum(M_svd_male$u[, 1]) 
                
                # females
                ax_female  <- rowSums(log(M_female))/ndates_in
                M_svd_female      <- svd(log(M_female)-ax_female)
                bx_female         <- M_svd_female$u[, 1]/sum(M_svd_female$u[, 1])
                # consistency for b(x) negatives
                bx_female[bx_female<0] <- 0
                bx_female         <- bx_female/sum(bx_female)
                kt_female         <- M_svd_female$d[1] * M_svd_female$v[, 1] * sum(M_svd_female$u[, 1]) 
                
                # common factor model
                common_cols <- intersect(colnames(M_male),colnames(M_female)) # some countries have error data and not equal btw sex
                M_both <- M_male[,common_cols] * male_weight + M_female[,common_cols] * (1-male_weight)
                ax_both  <- rowSums(log(M_both))/ndates_in
                M_svd_both      <- svd(log(M_male)-ax_male)
                # consistency for b(x) negatives
                Bx         <- M_svd_both$u[, 1]/sum(M_svd_both$u[, 1])
                Bx[Bx<0]   <- 0
                Bx         <- Bx/sum(Bx)
                Kt         <- M_svd_both$d[1] * M_svd_both$v[, 1] * sum(M_svd_both$u[, 1]) 
                
                # augmented common factor model
                # M_svd_ACF_male   <- svd(log(M_male) - ax_male - Bx %*% t(Kt))
                # bx_ACF_male        <- M_svd_ACF_male$u[, 1]/sum(M_svd_ACF_male$u[, 1])
                # kt_ACF_male        <- M_svd_ACF_male$d[1] * M_svd_ACF_male$v[, 1] * sum(M_svd_ACF_male$u[, 1]) 
                # M_svd_ACF_female   <- svd(log(M_female) - ax_female - Bx %*% t(Kt))
                # bx_ACF_female      <- M_svd_ACF_female$u[, 1]/sum(M_svd_ACF_female$u[, 1])
                # kt_ACF_female      <- M_svd_ACF_female$d[1] * M_svd_ACF_female$v[, 1] * sum(M_svd_ACF_female$u[, 1]) 

                # return
                return(list(M_male = M_male, M_female = M_female,
                                ax_male = ax_male, ax_female = ax_female, ax_both = ax_both,
                                bx_male = bx_male, bx_female = bx_female,
                                kt_male = kt_male, kt_female = kt_male, 
                                Bx = Bx, Kt = Kt))
                                # bx_ACF_male = bx_ACF_male, bx_ACF_female=bx_ACF_female, 
                                # kt_ACF_male=kt_ACF_male, kt_ACF_female=kt_ACF_female))
}

## de-duplicate function to rank and filter series based on different set of criteria (function took from PG)
deduplicates <- function(myDT) {
        # myDT <- country_db %>% as.data.table()
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
     if(!is.null(myDT$nrank3.x)){
             myDT$nrank3.x <- NULL
             myDT$nrank3 <- myDT$nrank3.y
             myDT$nrank3.y <- NULL        
     }
     return(myDT)
}

# download HMD data from UN server only useful indicators for plots
download_HMD_data <- function(dir){
        HMD_indicators <- c(
                250, # Probability of dying 15-60 (45q15)
                266, # E(x) - abridged
                229, # Infant mortality (1q0)
                239  # Under-five mortality (5q0)
        )
        # dir = "LifeTables/AuxFiles"
        HMD_data <- list()
        j = 1
        countries_in_HMD <- read.csv(file.path(dir,"countries_HMD.csv")) %>% pull()
        HMD_countries <- fertestr::locs_avail() %>% 
                inner_join(tibble(location_code_iso3 = countries_in_HMD))
        for(i in seq_along(HMD_countries$location_code)){
                # i = 752
                out <- get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),  
                                      # startYear = 1950,
                                      endYear = 2020,
                                      indicatorIds  = HMD_indicators,
                                      dataSourceShortNames = "HMD",
                                      dataSourceYears = 2021,
                                      locIds = as.integer(HMD_countries$location_code[i]),
                                      locAreaTypeIds = 2,
                                      subGroupIds = 2) %>% 
                        as.data.table() %>% deduplicates(.)
                HMD_data[[j]] = out 
                print(j)
                j = j + 1
        }
        # bind all
        HMD_data <- HMD_data %>% bind_rows(.id = "LocName")
        save(HMD_data, file = file.path(dir,"HMD_data.Rdata"))
}

# write output in InputFile (PG code)
write_InputFile <- function(input.file, life_table_age_sex, life_table_age_sex_abridged, mySeries){
        
        hs1 <- createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")
        
        ## header and column style for highlights
        editStyleHeader <- createStyle(halign = "CENTER", textDecoration = "Bold", border = "LeftRight", fontColour = "#FF0000", bgFill = "#FFFF00")
        editStyleColumn <- createStyle(halign = "CENTER", textDecoration = "Bold", border = "LeftRight", fontColour = "#FF0000")
        editStyleColumnBold <- createStyle(halign = "CENTER", textDecoration = "Bold", border = "LeftRight")
        
        ## style for comments
        s1 <- createStyle(fontSize = 12, fontColour = "red", textDecoration = c("BOLD"))
        s2 <- createStyle(fontSize = 10, fontColour = "black", textDecoration = c("BOLD"))
        
        ## style for formatting decimal values
        fm0 <- createStyle(numFmt = "0")
        fm1 <- createStyle(numFmt = "0.0")
        fm2 <- createStyle(numFmt = "0.00")
        fm3 <- createStyle(numFmt = "0.000")
        fm5 <- createStyle(numFmt = "0.00000")
        
        # load and write life_table_age_sex
        wb <- loadWorkbook(file = input.file)
        
        ## addWorksheet(wb, "life_table_age_sex2")
        ## writeDataTable(wb, sheet = "life_table_age_sex2",
        ##                x = life_table_age_sex %>% as.data.frame(),
        ##                colNames = TRUE, rowNames = FALSE, headerStyle = hs1, tableStyle = "TableStyleLight2", withFilter = FALSE, bandedRows = TRUE)
        ## addStyle(wb, "life_table_age_sex2", style = fm5, cols = 7, rows = 2:(nrow(life_table_age_sex)+1), gridExpand = TRUE)
        ## setColWidths(wb, "life_table_age_sex2", 1:ncol(life_table_age_sex), widths="auto")
        ## freezePane(wb, "life_table_age_sex2", firstRow = TRUE, firstCol = TRUE)
        ## removeWorksheet(wb, "life_table_age_sex")
        ## renameWorksheet(wb, "life_table_age_sex2", "life_table_age_sex")
        ## worksheetOrder(wb) <- c(1:17,24,18:23)
        
        # for testing only
        # life_table_age_sex <- readWorkbook(xlsxFile = wb, sheet = "life_table_age_sex")
        # replace values with 1 to test if the export works
        # life_table_age_sex$value <- 1
        
        openxlsx::deleteData(wb, sheet = "life_table_age_sex", cols = 1:ncol(life_table_age_sex_abridged), rows = 2:(nrow(life_table_age_sex_abridged)+1), gridExpand = TRUE)
        openxlsx::writeData(wb, sheet = "life_table_age_sex", x = life_table_age_sex_abridged, startCol = 1, startRow = 2, colNames = FALSE)
        
        openxlsx::deleteData(wb, sheet = "lt_abridged", cols = 1:ncol(life_table_age_sex), rows = 2:(nrow(life_table_age_sex)+1), gridExpand = TRUE)
        openxlsx::writeData(wb, sheet = "lt_abridged", x = life_table_age_sex, startCol = 1, startRow = 2, colNames = FALSE)
        
        # update_status worksheet
        update_status <- data.table(readWorkbook(xlsxFile = wb, sheet = "update_status"))
        # convert date columns
        changeCols <- c("created", "last_update", "last_global_run", "last_country_run", "mig_world_balance")
        update_status[,(changeCols):= lapply(.SD, as.character), .SDcols = changeCols]   
        now <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
        update_status[worksheet %in% c("mx5_raw", "lt_abridged", "life_table_age_sex"), last_update := now]
        ## update_status[worksheet %in% c("mx5_raw", "lt_abridged", "life_table_age_sex"), last_global_run := now]
        writeData(wb, sheet = "update_status", update_status$last_update, startCol=3, startRow=2, colNames = FALSE, rowNames=FALSE, headerStyle = hs1)
        ## writeData(wb, sheet = "update_status", update_status$last_global_run, startCol=4, startRow=2, colNames = FALSE, rowNames=FALSE, headerStyle = hs1)
        setColWidths(wb, "update_status", 1:ncol(update_status), widths="auto") 
        
        # dd_selected_series worksheet
        # note: SeriesID requires integer64 to preserve large numbers with more than 15 digits)
        # Excel uses a 64-bit floating point representation for numbers entered in cells, which means you can only have 15 digits precision.
        # the solution is to enter the ID numbers as text.
        dd_selected_series <- data.table(readWorkbook(xlsxFile = wb, sheet = "dd_selected_series"))
        ## our LT empirical data
        myIndicatorID <- unique(mySeries$IndicatorID)
        
        if (nrow(dd_selected_series)>0) {
                dd_selected_series <- dd_selected_series[(!IndicatorID %in% myIndicatorID) & is.na(IndicatorID)==FALSE]
        }
        dd_selected_series[, IndicatorID := as.integer(IndicatorID)]
        dd_selected_series <- rbind(dd_selected_series[is.na(IndicatorID)==FALSE], mySeries, fill=TRUE)
        dd_selected_series <- dd_selected_series[is.na(Status)==FALSE]
        
        ## removeWorksheet(wb, "dd_selected_series")
        ## addWorksheet(wb, sheet = "dd_selected_series")
        ## writeDataTable(wb, sheet = "dd_selected_series", x = dd_selected_series, colNames = TRUE, rowNames = FALSE, headerStyle = hs1, tableStyle = "TableStyleLight2", withFilter = FALSE, bandedRows = TRUE)
        
        openxlsx::deleteData(wb, sheet = "dd_selected_series", cols = 1:ncol(dd_selected_series), rows = 2:(nrow(dd_selected_series)+1), gridExpand = TRUE)
        openxlsx::writeData(wb, sheet = "dd_selected_series", x = dd_selected_series, startCol = 1, startRow = 2, colNames = FALSE)
        setColWidths(wb, "dd_selected_series", 1:ncol(dd_selected_series), widths="auto")
        freezePane(wb, "dd_selected_series", firstRow = TRUE, firstCol = TRUE)
        
        # save all changes
        saveWorkbook(wb, file = input.file, overwrite = T)
}

# OLD: plot rates with time-age variation
plot_age_time <- function(data, country = NULL){
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
                ggtitle(paste0("Rates by age and time. ",country))+
                theme_bw()+
                labs(colour="Age")+
                theme(legend.position="none")+
                facet_grid(~Sex)
}

# OLD: plot data availability
plot_data <- function(data, country=NULL){
        data <- data %>% filter(!str_detect(IndicatorName,"a\\(x"))
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
                geom_vline(xintercept = c(1950,2020),linetype=2)+
                theme_bw()+ 
                theme(legend.position="bottom")
}

# OLD:  plot dispersion measures
plot_dispersion <- function(data, country = NULL){
        minim_data <- floor(min(data$Date)/10)*10
        ggplot(data %>% 
                       pivot_longer(cols = 3:5, names_to = "Indicator")) + 
                geom_line(aes(x=Date, y=value, col=Sex))+
                facet_wrap(~Indicator,nrow = 2,ncol = 2, scales = "free")+
                geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
                geom_vline(xintercept = range(data$Date),linetype=2,col=2)+
                scale_x_continuous(name ="Year",
                                   breaks = seq(minim_data,2020.5,10), 
                                   labels = seq(minim_data,2020.5,10))+
                ggtitle(paste0("Dispertion measures. ",country))+
                theme_bw()
}

# OLD:  plot sex ratio measures
plot_sex_ratios <- function(data, country = NULL){
        minim_data <- floor(min(data$Date)/10)*10
        ggplot(data %>% 
                       pivot_longer(cols = 2:4, names_to = "Indicator")) + 
                geom_line(aes(x=Date, y=value, col=Indicator))+
                geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
                geom_vline(xintercept = range(data$Date),linetype=2,col=2)+
                scale_x_continuous(name ="Year",
                                   breaks = seq(minim_data,2020.5,10), 
                                   labels = seq(minim_data,2020.5,10))+
                ggtitle(paste0("Sex ratios at diferent ages. ",country))+
                theme_bw()
}

# OLD:  plot ex with time variation
plot_ex_time <- function(data, country = NULL){
        data$Date <- as.numeric(data$Date)
        data$Source <- as.factor(data$Source)
        minim_data <- floor(min(data$Date)/10)*10
        ggplot(data = data %>% filter(Age%in%c(0,65)),
               aes(x=as.numeric(Date),y=as.numeric(ex),
                   group=factor(Sex))) + 
                geom_line(col="grey")+
                geom_point(aes(shape=factor(Sex),col=Source)) +
                geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
                geom_vline(xintercept = range(data$Date),linetype=2,col=2)+
                scale_x_continuous(name ="Year",
                                   breaks = seq(1940.5,2020.5,10), 
                                   labels = seq(1940.5,2020.5,10))+
                labs(y="e(x)",colour = "Source",shape = "Sex")+
                ggtitle(paste0("Life expectancy at birth and age 65. ",country))+
                theme_bw()+
                theme(legend.position="bottom")+
                facet_wrap(~Age,nrow = 3, scales = "free_y",)
}

Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
}

# possible mortality laws: vector in mort_inputs
mort_laws_options <- c("kannisto",
               "kannisto_makeham",
               "cokannisto",
               "makeham",
               "gompertz",
               "ggompertz",
               "beard",
               "beard_makeham",
               "quadratic")
