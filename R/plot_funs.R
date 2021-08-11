# see available data
# XXX_LT1_XEmpirical-Data-Sources
# XXX_LT5_XEmpirical-Data-Sources

plot_data_selected <- function(name, data, selected_data, country=NULL){
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
                theme_light()+
                theme(legend.position="bottom")+
                annotate("segment",
                                 x = selected_data$TimeMid[-nrow(selected_data)],
                                 xend = selected_data$TimeMid[-1],
                                 y = selected_data$DataSourceShortName[-nrow(selected_data)],
                                 yend = selected_data$DataSourceShortName[-1],
                                 size=3, alpha=0.2) +
                        ggtitle(label = paste0("Source data from ",name),
                                subtitle = "Selected data with a line")
}

# comparison function
# XXX_ex_XLife-expectancy-Estimates-Time-Trend-comparison
# XXX_1q0_XInfant-mortality-Estimates-Time-Trend-comparison
# XXX_5q0_XUnder-five-mortality-Estimates-Time-Trend-comparison
# XXX_4q1_XChild-mortality-Estimates-vs-1q0-Time-Trend-comparison
# XXX_45q15_XAdult-mortality-Estimates-Time-Trend-comparison
# XXX_45q15_XAdult-mortality-Estimates-vs-5q0-Time-Trend-comparison
# XXX_e60_XOld-age-mortality-Estimates-vs-45q15-Time-Trend-comparison
# XXX_e80_XOld-age-mortality-Estimates-vs-45q15-Time-Trend-comparison

plot_comparison <- function(name, country_data, HMD_data,x = "e0&e80"){

        # set some vars
        country_data <- country_data %>% 
                mutate(Sex = factor(Sex, levels=c("m","f"), labels = c("Male","Female")))       
        
     if(x=="e0&e80"){
          data_plot <- HMD_data %>% 
               filter(AgeStart %in% c(0,60,80),
                      IndicatorName == "E(x) - abridged") %>% 
               select(Date=TimeMid,Age=AgeStart,ex=DataValue,Sex) 
          gg <- ggplot()+
                  geom_point(data = data_plot, aes(x=Date,y=ex), color = "grey", alpha=.5)+
                  geom_line(data = country_data %>% filter(Age %in% c(0,60,80)), 
                            aes(x=Date,y=ex), col=2)+
                  theme_minimal()+
                  labs(x="Year",y="e(x)") +
                  facet_grid(rows = vars(Age), 
                             cols = vars(Sex),
                             scales = "free_y",space = "free",switch = "y")+
                  ggtitle(paste0("Life expectancy at age 0, 60 and 80. HMD and ", name))
     }
        if(x==c("1q0")){
                data_plot <- HMD_data %>% 
                        filter(AgeStart == 0,
                               IndicatorName == "Infant mortality (1q0)") %>% 
                        select(Date=TimeMid,Age=AgeStart,nqx=DataValue,Sex=Sex) 
                gg <- ggplot()+
                        geom_point(data = data_plot, aes(x=Date,y=nqx), color = "grey", alpha=.5)+
                        geom_line(data = country_data %>% filter(Age == 0), 
                                  aes(x=Date,y=nqx), col=2)+
                        theme_minimal()+
                        labs(x="Year",y="1q0") +
                        facet_grid(cols = vars(Sex))+
                        ggtitle(paste0("Infant Mortality (1q0). HMD and ", name))
        }
        if(x==c("5q0")){
                data_plot <- HMD_data %>% 
                        filter(IndicatorName == "Under-five mortality (5q0)") %>% 
                        select(Date=TimeMid,Age=AgeStart,nqx=DataValue,Sex) 
                gg <- ggplot()+
                        geom_point(data = data_plot, aes(x=Date,y=nqx), color = "grey", alpha=.5)+
                        geom_line(data = country_data %>% filter(Age == 0), 
                                  aes(x=Date,y=nqx), col=2)+
                        theme_minimal()+
                        labs(x="Year",y="5q0") +
                        facet_grid(cols = vars(Sex))+
                        ggtitle(paste0("Child mortality (0q5). HMD and ", name))
        }
        if(x==c("1q0vs4q1")){
                data_plot <- HMD_data %>% 
                        filter(IndicatorName %in% c("Under-five mortality (5q0)","Infant mortality (1q0)")) %>% 
                        select(Date=TimeMid,IndicatorName,nqx=DataValue,Sex,LocName) %>% 
                        pivot_wider(names_from = IndicatorName, values_from = nqx) %>% 
                        rename(`1q0`=4,`5q0`=5) %>% 
                        mutate(`4q1`=(`5q0`-`1q0`)/(1-`1q0`))
                gg <- ggplot(data = country_data %>% 
                                     group_by(Source,Date,Sex) %>% 
                                     summarise(`1q0` = mean(nqx[Age==0]),
                                               `4q1` = mean(1-lx[Age==5]/lx[Age==1])),
                             aes(x=`1q0`,y=`4q1`))+
                        geom_point(data=data_plot, color = "grey", alpha=.5)+
                        geom_line( color=1) +
                        geom_point(aes(color=Source)) +
                        theme_light()+
                        labs(x="1q0",y="4q1") +
                        facet_grid(cols = vars(Sex))+
                        ggtitle(paste0("1q0 and 4q1. HMD and ", name))
        }
        if(x==c("45q15")){
                data_plot <- HMD_data %>% 
                        filter(IndicatorName == "Probability of dying 15-60 (45q15)") %>% 
                        select(Date=TimeMid,Age=AgeStart,nqx=DataValue,Sex) 
                gg <- ggplot(data_plot)+
                        geom_point(aes(x=Date,y=nqx), color = "grey", alpha=.5)+
                        geom_line(data = country_data %>% 
                                          group_by(Date,Sex) %>% 
                                          summarise(`45q15` = mean(1-lx[Age==60]/lx[Age==15])),
                                  aes(x=Date,y=`45q15`), color = 2)+
                        theme_light()+
                        theme(legend.position="bottom")+
                        labs(x="Date",y="45q15") +
                        facet_grid(cols = vars(Sex),
                                   scales = "free_y")+
                        ggtitle(paste0("Adult mortality (45q15). HMD and ", name))
        }
        if(x==c("45q15vs5q0")){
                data_plot <- HMD_data %>% 
                        filter(IndicatorName %in% c("Probability of dying 15-60 (45q15)",
                                                    "Under-five mortality (5q0)")) %>% 
                        select(Date=TimeMid,IndicatorName,nqx=DataValue,Sex,LocName)  %>% 
                        pivot_wider(names_from = IndicatorName, values_from = nqx) %>% 
                        rename(`45q15`=5,`5q0`=4)
                gg <- ggplot(data_plot)+
                        geom_point(aes(x=`5q0`,y=`45q15`), color = "grey", alpha=.5)+
                        geom_line(data = country_data %>% 
                                          group_by(Date,Sex) %>% 
                                          summarise(`45q15` = mean(1-lx[Age==60]/lx[Age==15]),
                                                    `5q0` = mean(1-lx[Age==5]/lx[Age==0])),
                                  aes(x=`5q0`,y=`45q15`), color = 2)+
                        theme_light()+
                        theme(legend.position="bottom")+
                        labs(x="5q0",y="45q15") +
                        facet_grid(cols = vars(Sex),
                                   scales = "free_y", space = "free",switch = "y")+
                        ggtitle(paste0("5q0 and 45q15. HMD and ", name))
        }
        if(x==c("e60vs45q15")){
                data_plot <- HMD_data %>% 
                        group_by(TimeMid,Sex,LocName)  %>%
                        summarise(e60 = unique(DataValue[AgeStart==60 & IndicatorName=="E(x) - abridged"])) %>% 
                        inner_join(
                        HMD_data %>% 
                        group_by(TimeMid,Sex,LocName)  %>%
                        summarise(`45q15` = unique(DataValue[AgeStart==15 & IndicatorName=="Probability of dying 15-60 (45q15)"])),
                        by = c("TimeMid", "Sex" ,  "LocName")
                        ) %>% select(Date=TimeMid,`45q15`,`e60`,Sex)
                gg <- ggplot(data_plot)+
                        geom_point(aes(x=`45q15`,y=`e60`), color = "grey", alpha=.5)+
                        geom_line(data = country_data %>% 
                                           group_by(Date,Sex) %>% 
                                           summarise(`45q15` = mean(1-lx[Age==60]/lx[Age==15]),
                                                     `e60` = mean(ex[Age==60])),
                                   aes(x=`45q15`,y=`e60`), color = 2)+
                        theme_light()+
                        theme(legend.position="bottom")+
                        labs(x="45q15",y="e60") +
                        facet_grid(cols = vars(Sex),
                                   scales = "free_y")+
                        ggtitle(paste0("45q15 and e60. HMD and ", name))
        }
        if(x==c("e80vs45q15")){
                data_plot <- HMD_data %>% 
                        group_by(TimeMid,Sex,LocName)  %>%
                        summarise(e80 = unique(DataValue[AgeStart==80 & IndicatorName=="E(x) - abridged"])) %>% 
                        inner_join(
                        HMD_data %>% 
                                group_by(TimeMid,Sex,LocName)  %>%
                                summarise(`45q15` = unique(DataValue[AgeStart==15 & IndicatorName=="Probability of dying 15-60 (45q15)"])),
                        by = c("TimeMid", "Sex" ,  "LocName")
                        ) %>% select(Date=TimeMid,`45q15`,`e80`,Sex)
                gg <- ggplot(data_plot)+
                        geom_point(aes(x=`45q15`,y=`e80`), color = "grey", alpha=.5)+
                        geom_line(data = country_data %>% 
                                          group_by(Date,Sex) %>% 
                                          summarise(`45q15` = mean(1-lx[Age==60]/lx[Age==15]),
                                                    `e80` = mean(ex[Age==80])),
                                  aes(x=`45q15`,y=`e80`), color = 2)+
                        theme_light()+
                        theme(legend.position="bottom")+
                        labs(x="45q15",y="e80") +
                        facet_grid(cols = vars(Sex),
                                   scales = "free_y")+
                        ggtitle(paste0("45q15 and e80. HMD and ", name))
        }
        return(gg)
}

# trend plots
# XXX_ex_XLife-expectancy-Estimates-Time-Trend
# XXX_Mx1_XEstimates-Time-Trend
# XXX_Mx1_XEstimates-Age-Profiles
# XXX_Mx1_XEstimates-Lexis-surface
# XXX_Mx1_XEstimates-Sex-Ratios-Time-Trend
# XXX_Mx1_XEstimates-Sex-Ratios-Age-Profiles
# XXX_Mx1_XEstimates-Sex-Ratios-Lexis-surface
plot_trends <- function(name, country_data, x = "ex"){
        # country_data <-  t_final_data
        country_data$Date <- as.numeric(country_data$Date)
        country_data$Source <- as.factor(country_data$Source)
        minim_data <- floor(min(country_data$Date)/10)*10
        
        if(x=="ex"){
                gg <- ggplot(data = country_data %>% filter(Age%in%c(0,60,80)),
                       aes(x=as.numeric(Date),y=as.numeric(ex))) + 
                        geom_line(col="grey")+
                        geom_point(aes(col=Source)) +
                        geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
                        geom_vline(xintercept = range(country_data$Date),linetype=2,col=2)+
                        scale_x_continuous(name ="Year",
                                           breaks = seq(1940,2020,10), 
                                           labels = seq(1940,2020,10))+
                        labs(y="e(x)",colour = "Source")+
                        theme_light()+
                        theme(legend.position="bottom")+
                        facet_grid(Age~Sex, scales = "free_y", space = "free",switch = "y")+
                        ggtitle(paste0("Life expectancy at age 0, 60 and 80. ",name))
        }
        if(x=="mx_time"){
                gg <- ggplot() + 
                        geom_line(data = country_data %>% filter(Type == "main"),
                                  aes(x=as.numeric(Date),y=nMx,
                                      col=factor(Age))) +
                        geom_point(data = country_data %>% filter(Type!="main"),
                                    aes(x=as.numeric(Date),y=nMx, col=factor(Age))) + 
                        geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
                        geom_vline(xintercept = range(country_data$Date),linetype=2,col=2)+
                        scale_x_continuous(name ="Year",
                                           breaks = seq(minim_data,2020,10), 
                                           labels = seq(minim_data,2020,10))+
                        scale_y_log10()+
                        theme_light()+
                        theme(legend.position="none")+
                        labs(colour="Age")+
                        facet_grid(~Sex)+
                        ggtitle(paste0("Time trend of age rates. ",name))
        }
        if(x=="mx_age"){
                gg <- ggplot() + 
                        geom_line(data = country_data,
                                  aes(x=Age,y=nMx,
                                      col=factor(Date))) +
                        scale_y_log10()+
                        facet_grid(~Sex) +
                        theme_light()+
                        theme(legend.position="none")+
                        ggtitle(paste0("Rates by age with time. ",name))
                
        }
        if(x=="lexis"){
                my_breaks = 10^seq(0,-7,by = -.5)
                gg <- country_data %>% 
                        ggplot(aes(x = Date, y = Age, fill = nMx)) + 
                        geom_tile() +
                        scale_fill_gradientn(colors = sequential_hcl(palette = "BurgYl",n=10) %>% rev(),
                                             trans= "log10", 
                                             labels = comma) +
                        coord_equal()+
                        theme_light()+
                        labs(fill="Mortality rate\n(log colors)")+
                        theme(legend.position="bottom")+
                        facet_grid(~Sex)+
                        ggtitle(paste0("Lexis surface of rates. ",name))
                
        }
        if(x=="sex_ratio_time"){
                data_sr = country_data %>% select(Date, Type, Age, Sex, nMx) %>% 
                        pivot_wider(names_from = Sex, values_from = nMx) %>% 
                        mutate(lrat =log(m / f),
                               lrat = case_when(lrat < -1 ~ -1,
                                                lrat > 1 ~ 1,
                                                TRUE ~ lrat))
                gg <- ggplot() + 
                        geom_line(data = data_sr %>% filter(Type == "main"),
                                  aes(x=as.numeric(Date),y=lrat,
                                      col=factor(Age))) +
                        geom_point(data = data_sr %>% filter(Type!="main"),
                                   aes(x=as.numeric(Date),y=lrat, col=factor(Age))) + 
                        geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
                        geom_vline(xintercept = range(country_data$Date),linetype=2,col=2)+
                        scale_x_continuous(name ="Year",
                                           breaks = seq(minim_data,2020,10), 
                                           labels = seq(minim_data,2020,10))+
                        theme_light()+
                        labs(colour="Age")+
                        theme(legend.position="none")+
                        ggtitle(paste0("Time trend of sex ratio of rates. ",name))
                
        }
        if(x=="sex_ratio_age"){
                data_sr = country_data %>% select(Date, Type, Age, Sex, nMx) %>% 
                        pivot_wider(names_from = Sex, values_from = nMx) %>% 
                        mutate(lrat =log(m / f),
                               lrat = case_when(lrat < -1 ~ -1,
                                                lrat > 1 ~ 1,
                                                TRUE ~ lrat)) %>% 
                        mutate(`log(m/f)`=lrat)
                gg <-ggplot() + 
                        geom_line(data = data_sr %>% filter(Type == "main"),
                                  aes(x=Age,y=`log(m/f)`,
                                      col=factor(Date))) +
                        geom_point(data = data_sr %>% filter(Type!="main"),
                                   aes(x=Age,y=`log(m/f)`,
                                       col=factor(Date)))+
                        theme_light()+
                        theme(legend.position="none")+
                        ggtitle(paste0("Sex ratio of rates by age with time. ",name))
                
        }
        if(x=="sex_ratio_lexis"){
                gg <- country_data %>% select(Date, Age, Sex, nMx) %>% 
                        pivot_wider(names_from = Sex, values_from = nMx) %>% 
                        mutate(lrat =log(m / f),
                               lrat = case_when(lrat < -1 ~ -1,
                                                lrat > 1 ~ 1,
                                                TRUE ~ lrat)) %>% 
                        mutate(`log(m/f)`=lrat) %>% 
                        ggplot(aes(x = Date, y = Age, fill = `log(m/f)`)) +
                        geom_tile() +
                        scale_fill_continuous_diverging("Blue-Red3") +
                        coord_equal()+
                        theme_light()+
                        theme(legend.position="bottom")+
                        ggtitle(paste0("Lexis surface of sex ratio [log(m/f)]. ",name))
        }
        return(gg)
}

# plot dispersion measures
plot_dispersion <- function(data, name = NULL){
        minim_data <- floor(min(data$Date)/10)*10
        ggplot(data %>% 
                       pivot_longer(cols = 3:5, names_to = "Indicator")) + 
                geom_line(aes(x=Date, y=value, col=Sex))+
                facet_wrap(~Indicator,nrow = 2,ncol = 2, scales = "free")+
                geom_vline(xintercept = c(1950.5,2020.5),linetype=2)+
                geom_vline(xintercept = range(data$Date),linetype=2,col=2)+
                scale_x_continuous(name ="Year",
                                   breaks = seq(minim_data,2020,10), 
                                   labels = seq(minim_data,2020,10))+
                ggtitle(paste0("Dispertion measures. ",name))+
                theme_light()
}
# XXX_Mx5_XEstimates-Time-Trend
# XXX_Mx5_XEstimates-Time-Trend-by-Age
# XXX_Mx5_XEstimates-Age-Profiles
# XXX_Mx5_XEstimates-Sex-Ratios-Time-Trend
# XXX_Mx5_XEstimates-Sex-Ratios-Age-Profiles