# author: IW
# set of functions used in `fill_gaps.R` and `fill_gaps_lt.R`.

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
                       DataReliabilitySort = NA) %>% 
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
                        validated <- coherence <- "not LT"
                        abr_ages <- c(0,1,seq(5,60,5)) 
                        # is a lifetable first, then wich one
                        if(str_detect(X$IndicatorName[1],"l\\(x|m\\(x")){
                                X <- X %>% filter(AgeStart<=100)
                                Age <- X$AgeStart
                                AgeInt <- X$AgeSpan
                                AgeInt[AgeInt<1] <- NA
                                pop_total <- pop %>% 
                                                filter(MidPeriod==unique(X$TimeMid)) %>% 
                                                summarise(sum(PopTotal)) %>% pull() * 1000
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
impute_data <- function(data, pop, k = 3, epsilon = NULL){

        # remove zeros
        if(is.null(epsilon)){
                epsilon <- data %>% 
                        filter(is_m_or_l(IndicatorName)=="m", DataValue>0) %>% 
                        summarise(min(DataValue)/2) %>% pull()
        }
        data <- data %>% 
                split(list(.$DataSourceShortName,.$TimeLabel,.$IndicatorName,
                           .$DataTypeSort,.$SexName), drop=T) %>% 
                  lapply(function(X){
                        # if(unique(X$TimeMid)==1990.5 & unique(X$SexName=="f")){browser()}
                        out <- X
                        if(!unique(X$validation) %in% c("not LT","error")){
                                if(is_m_or_l(unique(X$IndicatorName))=="m" & any(X$DataValue<=0)){
                                        # browser()
                                        out <- out %>% mutate(has_zero = ifelse(DataValue==0,1,0),
                                                      DataValue = ifelse(has_zero==1, DataValue+epsilon, DataValue),
                                                      DataAverage = zoo::rollmean(DataValue, k = k, fill = NA),
                                                      DataValue = ifelse(has_zero==1,DataAverage,DataValue),
                                                      validation = "rm_0s") %>% 
                                                select(-has_zero,-DataAverage)
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
                       (AgeStart == 2 | AgeStart == 3 | AgeStart == 4), 
                       validation == "abridged") %>%
                mutate(to_agr = 1) %>% 
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
        
        # output
        return(data_output %>% select(-ID))
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
        pop <- pop %>% filter(MidPeriod==floor(unique(x$TimeMid))+.5, AgeGrpStart<5) %>% 
                summarise(pop0_m = sum(m[AgeGrp==0]),
                          pop0_f = sum(f[AgeGrp==0]),
                          pop1_4_m = sum(m[AgeGrp %in% 1:4]),
                          pop1_4_f = sum(f[AgeGrp %in% 1:4]))
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
        source_herarchy <- tibble(DataSourceShortName = c("HMD","EuroStat","VR(WPP)","WHO DB",
                                                         "HLD 2020","DYB","GBD 2016","WPP19"),
                                  Index = 1:8)
        
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
                # if(year == 1950)browser()
                selection_y <- country_data %>%
                        filter(TimeMid_floor == year, validation!="not LT") %>%
                        group_by(DataSourceShortName, TimeMid_floor, TimeMid, TimeLabel, 
                                 IndicatorName, nrank3, DataTypeSort) %>% 
                        summarise(complete = is_single(unique(AgeStart)),
                                  OAG = max(AgeStart)) %>% 
                        mutate(is_avg_years = ifelse(substr(TimeLabel,6,9)=="",0,
                                                     as.integer(substr(TimeLabel,6,9))-
                                                     as.integer(substr(TimeLabel,1,4)))) %>% 
                        left_join(source_herarchy,by = "DataSourceShortName") %>%
                        left_join(lonely_points,by = c("DataSourceShortName","TimeMid_floor")) %>%
                        ungroup() %>% 
                        arrange(Lonely, # not lonely points
                                Index, # herarchy
                                nrank3, DataTypeSort, # this take care of HLD options
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
        desoverl <- desoverlap(option, country_data)
        
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
        
        # remove lonelys just next to serie
        final <- final %>% filter(!(Lonely==1 & (TimeMid-lead(TimeMid))==-1))
        
        # return
        return(final)
}

# decide on overlapping periods
desoverlap <- function(option, country_data, treshold = .02, ...){
        
        # detect overlaps
        overl <- option %>%
                group_by(TimeMid_floor) %>% mutate(id = row_number()) %>% 
                pivot_wider(names_from=id,values_from=DataSourceShortName) %>% 
                rename(ds1=2, ds2=3) %>% 
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

        # herarchy structure (again)
        source_herarchy <- tibble(DataSourceShortName = c("HMD","EuroStat","VR(WPP)","WHO DB",
                                                          "HLD 2020","DYB","GBD 2016","WPP19"),
                                  Index = 1:8)
        
        # set final ds for each
        desoverl <- overl %>% 
                split(list(.$ds1,.$ds2, .$groups), drop=T) %>% 
                lapply(function(X){
                        Xds1 <- unique(X$ds1)
                        Xds2 <- unique(X$ds2)
                        Date <- X$TimeMid
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
                                                       TimeMid_floor %in% date_prev, SexName=="f", 
                                                       str_detect(IndicatorName,"l\\(x")) %>% 
                                                lt_ambiguous(nMx_or_nqx_or_lx = .$DataValue,
                                                             type = "l", Sex = "f",
                                                             Age = .$AgeStart, Single = FALSE, ...)
                                        l1_post <- country_data %>% 
                                                filter(DataSourceShortName == ds_prev,
                                                       TimeMid_floor %in% date_post, SexName=="f", 
                                                       str_detect(IndicatorName,"l\\(x"))%>% 
                                                lt_ambiguous(nMx_or_nqx_or_lx = .$DataValue,
                                                             type = "l", Sex = "f",
                                                             Age = .$AgeStart, Single = FALSE, ...)
                                        l1_ds1 <- country_data %>% 
                                                filter(DataSourceShortName == Xds1,
                                                       TimeMid_floor %in% Date, SexName=="f", 
                                                       str_detect(IndicatorName,"l\\(x")) %>% 
                                                split(list(.$TimeMid)) %>% 
                                                lapply(function(X){
                                                        lt_ambiguous(nMx_or_nqx_or_lx = X$DataValue,
                                                                     type = "l", Sex = "f",
                                                                     Age = X$AgeStart, Single = FALSE, ...)         
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
                                                    age = 0:100,
                                                    sex = "m",
                                                    e0_target = e0_Males[j],
                                                    ...)$minimum
                        kt_female_star[j] <- optimize(f = interp_lc_lim_kt_min,
                                                      interval = c(-20, 20),
                                                      ax = params$ax_female,
                                                      bx = params$bx_female,
                                                      age = 0:100,
                                                      sex = "f",
                                                      e0_target = e0_Females[j],
                                                      ...)$minimum
                        Kt_star[j] <- optimize(f = interp_lc_lim_kt_min,
                                                      interval = c(-20, 20),
                                                      ax = params$ax_both,
                                                      bx = params$Bx,
                                                      age = 0:100,
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
                                                dates_in, dates_out,jump_off)
                     M_hat_females <- lc_forecast(params$ax_female,params$bx_female,params$kt_female,params$M_female,Sex="f",
                                                dates_in, dates_out,jump_off)
             }
        if(prev_cross_over){
                     M_hat_males   <- lc_forecast(params$ax_male,params$Bx,params$Kt,params$M_male, Sex="m",
                                                dates_in, dates_out,jump_off)
                     M_hat_females <- lc_forecast(params$ax_female,params$Bx,params$Kt,params$M_female, Sex="f",
                                                  dates_in, dates_out,jump_off)
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
lc_forecast <- function(ax, bx, kt, M, Sex, dates_in, dates_out, jump_off){
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
        M_hat$Age <- 0:100
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
        HMD_data <- list()
        j = 1
        countries_in_HMD <- read.csv(file.path(dir,"countries_HMD.csv")) %>% pull()
        HMD_countries <- fertestr::locs_avail() %>% 
                inner_join(tibble(location_code_iso3 = countries_in_HMD))
        for(i in seq_along(HMD_countries$location_code)){
                out <- get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),  
                                      startYear = 1950,
                                      endYear = 2020,
                                      indicatorIds  = HMD_indicators,
                                      dataSourceShortNames = "HMD",
                                      dataSourceYears = HMD_Year,
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
write_InputFile <- function(input.file, life_table_age_sex, mySeries){
        
        ## general table style for worksheet export/update
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
        wb <- loadWorkbook(input.file)
        
        addWorksheet(wb, "life_table_age_sex2")
        writeDataTable(wb, sheet = "life_table_age_sex2",
                       x = life_table_age_sex %>% as.data.frame(),
                       colNames = TRUE, rowNames = FALSE, headerStyle = hs1, tableStyle = "TableStyleLight2", withFilter = FALSE, bandedRows = TRUE)
        addStyle(wb, "life_table_age_sex2", style = fm5, cols = 7, rows = 2:(nrow(life_table_age_sex)+1), gridExpand = TRUE)
        setColWidths(wb, "life_table_age_sex2", 1:ncol(life_table_age_sex), widths="auto")
        freezePane(wb, "life_table_age_sex2", firstRow = TRUE, firstCol = TRUE)
        removeWorksheet(wb, "life_table_age_sex")
        renameWorksheet(wb, "life_table_age_sex2", "life_table_age_sex")
        worksheetOrder(wb) <- c(1:17,24,18:23)
        
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
        removeWorksheet(wb, "dd_selected_series")
        addWorksheet(wb, sheet = "dd_selected_series")
        writeDataTable(wb, sheet = "dd_selected_series", x = dd_selected_series, colNames = TRUE, rowNames = FALSE, headerStyle = hs1, tableStyle = "TableStyleLight2", withFilter = FALSE, bandedRows = TRUE)
        setColWidths(wb, "dd_selected_series", 1:ncol(dd_selected_series), widths="auto")
        freezePane(wb, "dd_selected_series", firstRow = TRUE, firstCol = TRUE)
        
        # save all changes
        saveWorkbook(wb, input.file, overwrite = T)
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
