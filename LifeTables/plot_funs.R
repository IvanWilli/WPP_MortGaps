# author: IW
# make the plots usign outputs from `fill_gaps_lt.R`. The list of plots is:
# Available data
        # XXX_LT1_XEmpirical-Data-Sources
# HMD comparison
        # XXX_ex_XLife-expectancy-Estimates-Time-Trend-comparison
        # XXX_1q0_XInfant-mortality-Estimates-Time-Trend-comparison
        # XXX_5q0_XUnder-five-mortality-Estimates-Time-Trend-comparison
        # XXX_4q1_XChild-mortality-Estimates-vs-1q0-Time-Trend-comparison
        # XXX_45q15_XAdult-mortality-Estimates-Time-Trend-comparison
        # XXX_45q15_XAdult-mortality-Estimates-vs-5q0-Time-Trend-comparison
        # XXX_e60_XOld-age-mortality-Estimates-vs-45q15-Time-Trend-comparison
        # XXX_e80_XOld-age-mortality-Estimates-vs-45q15-Time-Trend-comparison
# trend plots
        # XXX_ex_XLife-expectancy-Estimates-Time-Trend
        # XXX_Mx1_XEstimates-Time-Trend
        # XXX_Mx1_XEstimates-Age-Profiles
        # XXX_Mx1_XEstimates-Lexis-surface
        # XXX_Mx1_XEstimates-Sex-Ratios-Time-Trend
        # XXX_Mx1_XEstimates-Sex-Ratios-Age-Profiles
        # XXX_Mx1_XEstimates-Sex-Ratios-Lexis-surface
        # XXX_Mx1_XLife-expectancy-Gap-Estimates-Time-Trend
        # XXX_Mx1_ex_XLife-expectancy-Estimates-Time-Trend-Smoothing
        # XXX_Mx1_XEstimates-Lexis-surface-Smoothing
# abridged lt plots
        # XXX_Mx5_XEstimates-Time-Trend
        # XXX_Mx5_XEstimates-Age-Profiles
        # XXX_Mx5_XEstimates-Sex-Ratios-Time-Trend
        # XXX_Mx5_XEstimates-Sex-Ratios-Age-Profiles

# gral function for naming and wirting plots as svg
write_plots <- function(dir_plots, output, smoothing = "None", dir_HMD, complete_abridged){
        
        # rename
        # load("LifeTables/AuxFiles/BRA_output.Rdata")
        # dir_HMD <- "LifeTables/AuxFiles"
        # dir_plots <- "WPP2021_plots"
        name_code = output$name$Code_iso
        name = output$name$Name
        t_final_data = output$output_data
        country_data = output$input_data
        selected_data = output$selected_data
        data_pre_smooth = output$data_pre_smooth
        data_with_smooth = output$data_with_smooth
        country_data5 = output$output_data_abr
        post_old_age_adj <- output$old_age_adj_data
                
        # data form HMD for comparisons
        if(file.exists(file.path(dir_HMD,"HMD_data.Rdata"))){
                load(file.path(dir_HMD,"HMD_data.Rdata"))
                
        }else{
                download_HMD_data(dir = file.path(dir_HMD,"HMD_data.Rdata"))
        }
        
        HMD_data <- HMD_data %>% 
                filter(SexName != "Both sexes") %>% 
                mutate(Sex = factor(SexName, levels = c("Male","Female")))
                
        # write plots
        svg(file.path(dir_plots,paste0(name_code,"_LT1_XEmpirical-Data-Sources.svg")), width=22*.9, height=12*.9) 
        print(plot_data_selected(name, country_data, selected_data))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_ex_XLife-expectancy-Estimates-Time-Trend-comparison.svg")), width=22*.9, height=12*.9)
        print(plot_comparison(name, t_final_data, HMD_data, x = "e0&e60&e80"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_1q0_XInfant-mortality-Estimates-Time-Trend-comparison.svg")), width=22*.9, height=12*.9)
        try(print(plot_comparison(name, t_final_data, HMD_data,x = "1q0")))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_5q0_XUnder-five-mortality-Estimates-Time-Trend-comparison.svg")), width=22*.9, height=12*.9)
        try(print(plot_comparison(name, t_final_data, HMD_data,x = "5q0")))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_4q1_XChild-mortality-Estimates-vs-1q0-Time-Trend-comparison.svg")), width=22*.9, height=12*.9)
        print(plot_comparison(name, t_final_data, HMD_data,x = "1q0vs4q1"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_45q15_XAdult-mortality-Estimates-Time-Trend-comparison.svg")), width=22*.9, height=12*.9)
        try(print(plot_comparison(name, t_final_data, HMD_data,x = "45q15")))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_45q15_XAdult-mortality-Estimates-vs-5q0-Time-Trend-comparison.svg")), width=22*.9, height=12*.9)
        print(plot_comparison(name, t_final_data, HMD_data,x = "45q15vs5q0"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_e60_XOld-age-mortality-Estimates-vs-45q15-Time-Trend-comparison.svg")), width=22*.9, height=12*.9)
        print(plot_comparison(name, t_final_data, HMD_data,x = "e60vs45q15"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_e80_XOld-age-mortality-Estimates-vs-45q15-Time-Trend-comparison.svg")), width=22*.9, height=12*.9)
        print(plot_comparison(name, t_final_data, HMD_data,x = "e80vs45q15"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_ex_XLife-expectancy-Estimates-Time-Trend.svg")), width=22*.9, height=12*.9)
        print(plot_trends(name, t_final_data,x="ex"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_ex_XLife-expectancy-Gap-Estimates-Time-Trend.svg")), width=22*.9, height=12*.9)
        print(plot_trends(name, t_final_data,x="ex_gap"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx1_XEstimates-Time-Trend.svg")), width=22*.9, height=12*.9)
        print(plot_trends(name, t_final_data,x="mx_time"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx1_XEstimates-Age-Profiles.svg")), width=22*.9, height=12*.9)
        print(plot_trends(name, t_final_data,x="mx_age"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx1_XEstimates-Lexis-surface.svg")), width=22*.9, height=12*.9)
        print(plot_trends(name, t_final_data,x="lexis"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx1_XEstimates-Sex-Ratios-Time-Trend.svg")), width=22*.9, height=12*.9)
        try(print(plot_trends(name, t_final_data,x="sex_ratio_time")))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx1_XEstimates-Sex-Ratios-Age-Profiles.svg")), width=22*.9, height=12*.9)
        print(plot_trends(name, t_final_data,x="sex_ratio_age"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx1_XEstimates-Sex-Ratios-Lexis-surface.svg")), width=22*.9, height=12*.9)
        print(plot_trends(name, t_final_data,x="sex_ratio_lexis"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_ex_XLife-expectancy-Estimates-Time-Trend-Smoothing.svg")), width=22*.9, height=12*.9)
        print(plot_trends(name, data_pre_smooth, x="ex_smooth", data_with_smooth, smoothing))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx1_XEstimates-Time-Trend-Smoothing.svg")), width=22*.9, height=12*.9)
        print(plot_trends(name, data_pre_smooth, x="mx_time_smooth", data_with_smooth, smoothing))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx1_XEstimates-Lexis-surface-Smoothing.svg")), width=22*.9, height=12*.9)
        print(plot_trends(name, data_pre_smooth, x="lexis_smooth", data_with_smooth, smoothing))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx5_XEstimates-Time-Trend.svg")), width=22*.9, height=12*.9)
        print(plot_trends5(name, country_data5,x="mx5_time"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx5_XEstimates-Age-Profiles.svg")), width=22*.9, height=12*.9)
        print(plot_trends5(name, country_data5,x="mx5_age"))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx5_XEstimates-Sex-Ratios-Time-Trend.svg")), width=22*.9, height=12*.9)
        try(print(plot_trends5(name, country_data5,x="sex_ratio_time5")))
        dev.off()
        svg(file.path(dir_plots,paste0(name_code,"_Mx5_XEstimates-Sex-Ratios-Age-Profiles.svg")), width=22*.9, height=12*.9)
        print(plot_trends5(name, country_data5,x="sex_ratio_age5"))
        dev.off()
        if(complete_abridged == "Abridged"){
                svg(file.path(dir_plots,paste0(name_code,"_Mx5_XEstimates-Old-Ages-Time-Trend.svg")), width=22*.9, height=12*.9)
                print(plot_old_age(name, country_data, post_old_age_adj, selected_data))
                dev.off()        
        }
}

# plot available and selected data
plot_data_selected <- function(name, data, selected_data, country=NULL){
        
        personal_theme = theme(plot.title = element_text(hjust = 0.5),
                               text = element_text(size = 16),
                               plot.margin=grid::unit(c(0,0,0,0), "mm"),
                               plot.caption = element_text(color = 1, face = "italic", size=11))
        data <- data %>% filter(!str_detect(IndicatorName,"a\\(x"),
                                str_detect(IndicatorName, "\\(x"))
        selected_data_f <- selected_data %>% filter(SexName=="f")
        selected_data_m <- selected_data %>% filter(SexName=="m")
        data$IndicatorName <- factor(data$IndicatorName)
        dodge <- position_dodge(width=0.5)
        data %>% distinct(IndicatorName,DataSourceShortName,TimeMid) %>% 
                ggplot(aes(x=TimeMid,y=DataSourceShortName)) +
                geom_point(aes(color=IndicatorName,shape=IndicatorName),position = dodge)+
                scale_colour_viridis_d()+
                scale_shape_manual(values=1:nlevels(data$IndicatorName)) +
                scale_x_continuous(name ="Year",
                                   limits = c(1940,2020),
                                   breaks = seq(1940,2020,10), 
                                   labels = seq(1940,2020,10))+
                geom_vline(xintercept = c(1950,2020),linetype=2)+
                theme_bw()+
                labs(y="Data Source", caption = Sys.time()) +
                annotate("segment",
                                 x = selected_data_f$TimeMid[-nrow(selected_data_f)],
                                 xend = selected_data_f$TimeMid[-1],
                                 y = selected_data_f$DataSourceShortName[-nrow(selected_data_f)],
                                 yend = selected_data_f$DataSourceShortName[-1],
                                 size=3, alpha=0.2, col=2) +
                annotate("segment",
                                 x = selected_data_m$TimeMid[-nrow(selected_data_m)],
                                 xend = selected_data_m$TimeMid[-1],
                                 y = selected_data_m$DataSourceShortName[-nrow(selected_data_m)],
                                 yend = selected_data_m$DataSourceShortName[-1],
                                 size=3, alpha=0.2, col=4) +
                ggtitle(label = paste0("Source data from ",name),
                        subtitle = "Selected data with a line") +
                personal_theme
}

# plot comparisons against HMD
plot_comparison <- function(name, country_data, HMD_data,x = NULL){

        # set some vars
        country_data <- country_data %>% 
                mutate(Sex = factor(Sex, levels=c("m","f"), labels = c("Male","Female")),
                       label = ifelse(Date %in% seq(1950.5,2020.5,10),floor(Date),"")) %>% 
                rename(`Source/Method`=Source)
        # my theme
        personal_theme <- theme(plot.title = element_text(hjust = 0.5),
                               text = element_text(size = 16),
                               plot.margin=grid::unit(c(0,0,0,0), "mm"),
                               plot.caption = element_text(color = 1, face = "italic", size=11))
        
     if(x=="e0&e60&e80"){
          data_plot <- HMD_data %>% 
               filter(AgeStart %in% c(0,60,80),
                      IndicatorName == "E(x) - abridged") %>% 
               select(Date=TimeMid,Age=AgeStart,ex=DataValue,Sex) 
          gg <- ggplot(data = country_data %>% filter(Age %in% c(0,60,80)),
                       aes(x=Date,y=ex))+
                  geom_point(data = data_plot, aes(x=Date,y=ex), color = "grey", alpha=.3)+
                  scale_colour_viridis_d()+
                  geom_line(col=1, size = .3)+
                  geom_point(aes(color=`Source/Method`,shape=`Source/Method`))+
                  theme_bw()+
                  labs(x="Year",y=TeX("$e_x$"), caption = Sys.time()) +
                  scale_x_continuous(breaks = seq(1950,2020,10), 
                                     labels = seq(1950,2020,10))+
                  facet_grid(rows = vars(Age), 
                             cols = vars(Sex),
                             scales = "free_y",space = "free",switch = "y")+
                  ggtitle(paste0("Life expectancy at age 0, 60 and 80. HMD and ", name)) +
                  personal_theme
     }
        if(x==c("1q0")){
                data_plot <- HMD_data %>% 
                        filter(AgeStart == 0,
                               IndicatorName == "Infant mortality (1q0)") %>% 
                        select(Date=TimeMid,Age=AgeStart,nqx=DataValue,Sex=Sex) 
                gg <- ggplot(data = country_data %>% filter(Age == 0), 
                             aes(x=Date,y=nqx))+
                        geom_point(data = data_plot, aes(x=Date,y=nqx), color = "grey", alpha=.5)+
                        geom_line(col=1, size=.3)+
                        geom_point(aes(color=`Source/Method`,shape=`Source/Method`))+
                        scale_colour_viridis_d()+
                        theme_bw()+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        scale_y_continuous(labels = comma)+
                        labs(x="Year",y=TeX("$q_0$"), caption = Sys.time()) +
                        facet_grid(cols = vars(Sex))+
                        ggtitle(TeX(paste0("Infant Mortality ($q_0$). HMD and ", name))) +
                        personal_theme
        }
        if(x==c("5q0")){
                data_plot <- HMD_data %>% 
                        filter(IndicatorName == "Under-five mortality (5q0)") %>% 
                        select(Date=TimeMid,Age=AgeStart,nqx=DataValue,Sex) 
                gg <- ggplot(data = country_data %>% 
                                     group_by(`Source/Method`,Date,Sex) %>% 
                                     summarise(`5q0` = mean(1-lx[Age==5]/lx[Age==0])),
                             aes(x=Date,y=`5q0`))+
                        geom_point(data = data_plot, aes(x=Date,y=nqx), color = "grey", alpha=.5)+
                        geom_line(col=1, size=.3)+
                        geom_point(aes(color=`Source/Method`,shape=`Source/Method`))+
                        scale_colour_viridis_d()+
                        theme_bw()+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        scale_y_continuous(labels = comma)+
                        labs(x="Year",y=TeX("${}_5q_0$"), caption = Sys.time()) +
                        facet_grid(cols = vars(Sex))+
                        ggtitle(TeX(paste0("Child mortality (${}_5q_0$). HMD and ", name))) +
                        personal_theme
        }
        if(x==c("1q0vs4q1")){
                data_plot <- HMD_data %>% 
                        filter(IndicatorName %in% c("Under-five mortality (5q0)","Infant mortality (1q0)")) %>% 
                        select(Date=TimeMid,IndicatorName,nqx=DataValue,Sex,LocName) %>% 
                        pivot_wider(names_from = IndicatorName, values_from = nqx) %>% 
                        rename(`1q0`=4,`5q0`=5) %>% 
                        mutate(`4q1`=(`5q0`-`1q0`)/(1-`1q0`))
                gg <- ggplot(data = country_data %>% 
                                     group_by(`Source/Method`,Date,Sex,label) %>% 
                                     summarise(`1q0` = mean(nqx[Age==0]),
                                               `4q1` = mean(1-lx[Age==5]/lx[Age==1])),
                             aes(x=`1q0`,y=`4q1`)) +
                        geom_point(data=data_plot, color = "grey", alpha=.5) +
                        geom_point(aes(color=`Source/Method`,shape=`Source/Method`)) +
                        scale_colour_viridis_d()+
                        ggrepel::geom_text_repel(aes(label=label), size=3) +
                        theme_bw() +
                        scale_y_log10(labels = comma) +
                        scale_x_log10(labels = comma) +
                        theme(legend.position="bottom")+
                        labs(x=TeX("${}_1q_0$"),y=TeX("${}_4q_1$"), caption = Sys.time()) +
                        facet_grid(cols = vars(Sex)) +
                        ggtitle(paste0("infant and child mortality. HMD and ", name,". Log-scales")) +
                        personal_theme
        }
        if(x==c("45q15")){
                data_plot <- HMD_data %>% 
                        filter(IndicatorName == "Probability of dying 15-60 (45q15)") %>% 
                        select(Date=TimeMid,Age=AgeStart,`45q15`=DataValue,Sex) 
                gg <- ggplot(data = country_data %>% 
                                     group_by(`Source/Method`,Date,Sex) %>% 
                                     summarise(`45q15` = mean(1-lx[Age==60]/lx[Age==15])),
                             aes(x=Date,y=`45q15`))+
                        geom_point(data = data_plot, aes(x=Date,y=`45q15`), color = "grey", alpha=.5)+
                        geom_line(col=1, size=.3)+
                        geom_point(aes(color=`Source/Method`,shape=`Source/Method`))+
                        scale_colour_viridis_d()+
                        theme_bw()+
                        scale_y_continuous(labels = comma)+
                        theme(legend.position="bottom")+
                        labs(x="Year",y=TeX("${}_{45}q_{15}$"), caption = Sys.time()) +
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        facet_grid(cols = vars(Sex))+
                        ggtitle(TeX(paste0("Young-Adult mortality (${}_{45}q_{15}$). HMD and ", name))) +
                        personal_theme
        }
        if(x==c("45q15vs5q0")){
                data_plot <- HMD_data %>% 
                        filter(IndicatorName %in% c("Probability of dying 15-60 (45q15)",
                                                    "Under-five mortality (5q0)")) %>% 
                        select(Date=TimeMid,IndicatorName,nqx=DataValue,Sex,LocName)  %>% 
                        pivot_wider(names_from = IndicatorName, values_from = nqx) %>% 
                        rename(`45q15`=5,`5q0`=4)
                gg <- ggplot(country_data %>% 
                                     group_by(Date,Sex,label,`Source/Method`) %>% 
                                     summarise(`45q15` = mean(1-lx[Age==60]/lx[Age==15]),
                                               `5q0` = mean(1-lx[Age==5]/lx[Age==0])),
                             aes(x=`5q0`,y=`45q15`))+
                        geom_point(data = data_plot, aes(x=`5q0`,y=`45q15`), color = "grey", alpha=.5)+
                        geom_line(col=1, size=.3)+
                        geom_point(aes(color=`Source/Method`,shape=`Source/Method`))+
                        scale_colour_viridis_d()+
                        ggrepel::geom_text_repel(aes(label=label), size=3) +
                        theme_bw()+
                        scale_y_log10(labels = comma) +
                        scale_x_log10(labels = comma) +
                        theme(legend.position="bottom")+
                        labs(x=TeX("${}_5q_0$"),y=TeX("${}_45q_15$"), caption = Sys.time()) +
                        facet_grid(cols = vars(Sex),
                                   scales = "free_y", space = "free",switch = "y")+
                        ggtitle(paste0("Child and Young-Adult mortality. HMD and ", name,". Log-scales")) +
                        personal_theme
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
                gg <- ggplot(country_data %>% 
                                     group_by(Date,Sex,`Source/Method`,label) %>% 
                                     summarise(`45q15` = mean(1-lx[Age==60]/lx[Age==15]),
                                               `e60` = mean(ex[Age==60])),
                             aes(x=`45q15`,y=`e60`))+
                        geom_point(data = data_plot,aes(x=`45q15`,y=`e60`), color = "grey", alpha=.5)+
                        scale_colour_viridis_d()+
                        geom_point(aes(color=`Source/Method`,shape=`Source/Method`))+
                        ggrepel::geom_text_repel(aes(label=label), size=3) +
                        theme_bw()+
                        scale_x_continuous(labels = comma)+
                        theme(legend.position="bottom")+
                        labs(x=TeX("${}_{45}q_{15}$"),y=TeX("$e_{60}$"), caption = Sys.time()) +
                        facet_grid(cols = vars(Sex),
                                   scales = "free_y")+
                        ggtitle(paste0("45q15 and e60. HMD and ", name)) +
                        personal_theme
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
                gg <- ggplot(country_data %>% 
                                     group_by(Date,Sex,`Source/Method`,label) %>% 
                                     summarise(`45q15` = mean(1-lx[Age==60]/lx[Age==15]),
                                               `e80` = mean(ex[Age==80])),
                             aes(x=`45q15`,y=`e80`))+
                        geom_point(data = data_plot,aes(x=`45q15`,y=`e80`), color = "grey", alpha=.5)+
                        scale_colour_viridis_d()+
                        geom_point(aes(color=`Source/Method`,shape=`Source/Method`))+
                        ggrepel::geom_text_repel(aes(label=label), size=3) +
                        theme_bw()+
                        scale_x_continuous(labels = comma)+
                        theme(legend.position="bottom")+
                        labs(x=TeX("${}_{45}q_{15}$"),y=TeX("$e_{80}$"), caption = Sys.time()) +
                        facet_grid(cols = vars(Sex),
                                   scales = "free_y")+
                        ggtitle(paste0("45q15 and e80. HMD and ", name)) +
                        personal_theme
        }
        return(gg)
}

# plot time series trends
plot_trends <- function(name, country_data, x = NULL, country_data_smooth = NULL, smoothing="None"){
        # set some vars
        
        country_data <- country_data %>% 
                mutate(Sex = factor(Sex, levels=c("m","f"), labels = c("Male","Female")),
                       label = ifelse(Date %in% c(1950.5,2020.5), floor(Date),"")) %>% 
                rename(`Source/Method`=Source)
        
        # my theme
        personal_theme <- theme(plot.title = element_text(hjust = 0.5),
                                text = element_text(size = 16),
                                plot.margin=grid::unit(c(0,0,0,0), "mm"),
                                plot.caption = element_text(color = 1, face = "italic", size=11))
        
        if(x=="ex_gap"){
                gg <- ggplot(data = country_data %>% 
                                     filter(Age%in%c(0,60,80)) %>% 
                                     select(Age,Sex,`Source/Method`,Date,ex) %>% 
                                     pivot_wider(names_from=Sex, values_from=ex) %>% 
                                     mutate(Gap = Male/Female) %>% 
                                     pivot_longer(!Age:Date,names_to = "Sex",values_to = "Value") %>% 
                                     mutate(Measure = ifelse(Sex=="Gap","Gap(M/F)","Sex")),
                             aes(x=Date,y=Value)) + 
                        # geom_line(col="grey")+
                        geom_point(aes(col=Sex,shape=`Source/Method`)) +
                        labs(y=TeX("$e_{x}$"),x="Year", caption = Sys.time())+
                        theme_bw()+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        scale_colour_viridis_d()+
                        theme(legend.position="bottom")+
                        facet_grid(rows = vars(Measure), cols = vars(Age), 
                                   scales = "free")+
                        ggtitle(paste0("Life expectancy and sex gap as ratio (M/F). ",name)) +
                        personal_theme
        }
        if(x=="ex"){
                gg <- ggplot(data = country_data %>% filter(Age%in%c(0,60,80)),
                       aes(x=Date,y=ex)) + 
                        geom_line(col="grey")+
                        geom_point(aes(col=`Source/Method`,shape=`Source/Method`)) +
                        labs(y=TeX("$e_{x}$"),x="Year", caption = Sys.time())+
                        theme_bw()+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        scale_colour_viridis_d()+
                        theme(legend.position="bottom")+
                        facet_grid(Age~Sex, scales = "free_y", space = "free",switch = "x")+
                        ggtitle(paste0("Life expectancy at age 0, 60 and 80. ",name)) +
                        personal_theme
        }
        if(x=="mx_time"){
                country_data <- country_data %>% 
                        mutate(label = ifelse(Age %in% c(0,5,10,15,20,30,40,50,60,70,80,90,100) & Date == 1950.5, Age,""))

                gg <- ggplot(data = country_data,
                             aes(x=Date,y=nMx,
                                 col=Age, group=Age,label=label)) + 
                        geom_line() +
                        ggrepel::geom_text_repel(size=3, col="blue4",
                                                 seed = 42, box.padding = 0.5,
                                                 min.segment.length = 0, segment.color="blue4",max.overlaps = Inf,
                                                 point.size = NA) +
                        scale_y_log10(labels = comma)+
                        scale_colour_viridis_c(option = "B")+
                        labs(y=TeX("$log({}_nm_{x})$"), caption = Sys.time())+
                        theme_bw()+
                        labs(colour="Age", x="Year")+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        facet_grid(~Sex)+
                        ggtitle(paste0("Time trend of mortality rates by age. ",name)) + 
                        personal_theme
        }
        if(x=="mx_age"){
                country_data <- country_data %>% 
                        mutate(label = ifelse(Date %in% seq(1950.5,2020.5,5) & Age == 50, floor(Date),""),
                               Age = as.numeric(Age)) %>% 
                        select(Age, Date, nMx, Sex, label)
                gg <- ggplot(data = country_data,
                             aes(x=Age,y=nMx,
                                 col=Date, group=Date,label=label)) + 
                        geom_line() +
                        ggrepel::geom_text_repel(size=3, col="blue4",
                                                 seed = 42, box.padding = 0.5,
                                                 min.segment.length = 0, segment.color="blue4",max.overlaps = Inf,
                                                 point.size = NA) +
                        scale_y_log10(labels = comma)+
                        scale_colour_viridis_c(option = "B")+
                        labs(y=TeX("${}_nm_{x}$"),colour="Year", caption = Sys.time())+
                        theme_bw()+
                        scale_x_continuous(breaks = seq(0,100,10),
                                           labels = seq(0,100,10))+
                        facet_grid(~Sex)+
                        ggtitle(paste0("Mortality rates by age with time. ",name)) + 
                        personal_theme
        }
        if(x=="lexis"){
                my_breaks = 10^seq(0,-7,by = -.5)
                gg <- country_data %>% 
                        ggplot(aes(x = Date, y = Age, fill = nMx, z = nMx)) + 
                        geom_tile() +
                        geom_contour(breaks = my_breaks, color = gray(.8), alpha = 50, size = .5) +
                        scale_fill_gradientn(colors = sequential_hcl(palette = "Viridis",n=10) %>% rev(),
                                             trans= "log10", 
                                             labels = comma) +
                        coord_equal()+
                        theme_bw()+
                        labs(fill="Mortality rate\n(log colors)", x="Year", caption = Sys.time())+
                        scale_y_continuous(name ="Age",
                                           breaks = seq(0,100,10), 
                                           labels = seq(0,100,10))+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        facet_grid(~Sex)+
                        ggtitle(paste0("Lexis surface of mortality rates. ",name)) +
                        personal_theme +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        }
        if(x=="sex_ratio_time"){
                data_sr <- country_data %>% 
                        select(Date, `Source/Method`,Age, Sex, nMx) %>% 
                        pivot_wider(names_from = Sex, values_from = nMx) %>% 
                        mutate(rat = pmin(Male/Female,5),
                               label = ifelse(Age %in% c(0,5,10,15,20,30,40,50,60,70,80,90,100) & Date == 1950.5, Age,""))
                gg <- ggplot(data_sr, aes(x=as.numeric(Date),y=rat,
                                          col=Age,group=Age,label=label)) + 
                        geom_line() +
                        geom_hline(yintercept = 1,linetype=2)+
                        scale_colour_viridis_c(option = "B")+
                        ggrepel::geom_text_repel(size=3, col="blue4",
                                                 seed = 42, box.padding = 0.5,
                                                 min.segment.length = 0, segment.color="red",max.overlaps = Inf,
                                                 point.size = NA) +
                        theme_bw()+
                        labs(colour="Age", x="Year",y="M/F ratio", caption = Sys.time())+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        ggtitle(TeX(paste0("Time trend of sex ratio of mortality rates (male/female). ",name))) +
                        personal_theme
        }
        if(x=="sex_ratio_age"){
                data_sr <- country_data %>% select(Date, Age, Sex, nMx) %>% 
                        pivot_wider(names_from = Sex, values_from = nMx) %>% 
                        mutate(rat =pmin(Male/Female,5)) %>% 
                        mutate(label = ifelse(Date %in% seq(1950.5,2020.5,5) & Age == 90, floor(Date),""),
                               Age = as.numeric(Age))

                gg <-ggplot(data = data_sr,
                            aes(x=Age,y=rat,
                                col=Date, group = Date, label=label)) + 
                        geom_line() +
                        ggrepel::geom_text_repel(size=3, col="blue4",
                                                 seed = 42, box.padding = 0.5,
                                                 min.segment.length = 0, segment.color="blue4",max.overlaps = Inf,
                                                 point.size = NA) +
                        geom_hline(yintercept = 1,linetype=2)+
                        scale_colour_viridis_c(option = "A")+
                        theme_bw()+
                        labs(y="M/F ratio", caption = Sys.time())+
                        scale_x_continuous(name ="Age",
                                           breaks = seq(0,100,10), 
                                           labels = seq(0,100,10))+
                        ggtitle(paste0("Sex ratio (male/female) of mortality rates by age with time. ",name)) +
                        personal_theme
                
        }
        if(x=="sex_ratio_lexis"){
                my_breaks = seq(0,4,by = 1)
                gg <- country_data %>% select(Date, Age, Sex, nMx) %>% 
                        pivot_wider(names_from = Sex, values_from = nMx) %>% 
                        mutate(rat =pmin(Male/Female,5)) %>% 
                        ggplot(aes(x = Date, y = Age, fill =rat,z =rat)) +
                        geom_tile() +
                        geom_contour(breaks = my_breaks, color = 1, alpha = 50, size = .5) +
                        scale_fill_continuous_sequential("Heat") +
                        scale_y_continuous(name ="Age",
                                           breaks = seq(0,100,10), 
                                           labels = seq(0,100,10))+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        labs(x="Year", fill="M/F ratio", caption = Sys.time())+
                        coord_equal()+
                        theme_bw()+
                        ggtitle(paste0("Lexis surface of sex ratio of mortality rates (male/female). ",name)) +
                        personal_theme +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        }
        if(x == "ex_smooth"){
                country_data_compare <- bind_rows(
                                country_data %>% mutate(Smooth = "No Smoothing"),
                                country_data_smooth %>% mutate(Smooth = Model,
                                                        Sex = factor(Sex, levels=c("m","f"), labels = c("Male","Female")))) %>% 
                                mutate(Smooth = as.factor(Smooth))
                
                selected_smoothing = ifelse(tolower(smoothing) == "apc","APC", 
                                            ifelse(tolower(smoothing) == "ap", "AP", "Not smoothed in output")) 
                
                gg <- ggplot(data = country_data_compare %>% filter(Age%in%c(0,60,80)),
                       aes(x=Date,y=ex)) + 
                        geom_line(aes(col=Smooth))+
                        geom_point(aes(col=Smooth,shape=Smooth)) +
                        labs(y=TeX("$e_{x}$"),x="Year", caption = Sys.time())+
                        theme_bw()+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        scale_colour_viridis_d()+
                        theme(legend.position="bottom")+
                        facet_grid(Age~Sex, scales = "free_y", space = "free",switch = "x")+
                        ggtitle(paste0("Smoothed/Not Smoothed life expectancy. ",name),
                                subtitle = paste0("Selected: ", selected_smoothing," (excluding mortality crises)")) +
                        personal_theme +
                        theme(plot.subtitle = element_text(color = "red"))
        }
        if(x=="lexis_smooth"){
                country_data_compare <- bind_rows(
                        country_data %>% mutate(Smooth = "No Smoothing"),
                        country_data_smooth %>% mutate(Smooth = Model,
                                                       Sex = factor(Sex, levels=c("m","f"), labels = c("Male","Female")))) %>% 
                        mutate(Smooth = as.factor(Smooth))
                
                selected_smoothing = ifelse(tolower(smoothing) == "apc","APC", 
                                            ifelse(tolower(smoothing) == "ap", "AP", "Not smoothed in output")) 
                
                my_breaks = 10^seq(0,-7,by = -.5)
                gg <- country_data_compare %>% 
                        ggplot(aes(x = Date, y = Age, fill = nMx, z = nMx)) + 
                        geom_tile() +
                        geom_contour(breaks = my_breaks, color = gray(.8), alpha = 50, size = .5) +
                        scale_fill_gradientn(colors = sequential_hcl(palette = "Viridis",n=10) %>% rev(),
                                             trans= "log10", 
                                             labels = comma) +
                        coord_equal()+
                        theme_bw()+
                        labs(fill="Mortality rate\n(log colors)", x="Year", caption = Sys.time())+
                        scale_y_continuous(name ="Age",
                                           breaks = seq(0,100,10), 
                                           labels = seq(0,100,10))+
                        scale_x_continuous(breaks = seq(1950,2020,20), 
                                           labels = seq(1950,2020,20))+
                        facet_grid(rows=vars(Sex),cols=vars(Smooth))+
                        ggtitle(paste0("Smoothed/Not Smoothed Lexis surface of mortality rates. ",name),
                                subtitle = paste0("Selected: ", selected_smoothing," (excluding mortality crises)")) +
                        theme(plot.subtitle = element_text(color = "red")) +
                        personal_theme  +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        }
        if(x=="mx_time_smooth"){
                
                country_data_compare <- bind_rows(
                        country_data %>% mutate(Smooth = "No Smoothing"),
                        country_data_smooth %>% 
                                mutate(Smooth = Model,
                                        Sex = factor(Sex, levels=c("m","f"), labels = c("Male","Female")))) %>% 
                        mutate(Smooth = as.factor(Smooth),
                               label = ifelse(Age %in% c(0,5,10,15,20,30,40,50,60,70,80,90,100) & Date == 1950.5, Age,""))
                
                selected_smoothing = ifelse(tolower(smoothing) == "apc","APC", 
                                            ifelse(tolower(smoothing) == "ap", "AP", "Not smoothed in output")) 
                
                gg <- ggplot(data = country_data_compare,
                             aes(x=Date,y=nMx,
                                 col=Age, group=Age,label=label)) + 
                        geom_line() +
                        ggrepel::geom_text_repel(size=3, col="blue4",
                                                 seed = 42, box.padding = 0.5,
                                                 min.segment.length = 0, segment.color="blue4") +
                        scale_y_log10(labels = comma)+
                        scale_colour_viridis_c(option = "B")+
                        labs(y=TeX("$log({}_nm_{x})$"), caption = Sys.time())+
                        theme_bw()+
                        labs(colour="Age", x="Year")+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        facet_grid(rows=vars(Sex),cols=vars(Smooth))+
                        ggtitle(paste0("Time trend of mortality rates by age. ",name),
                                subtitle = paste0("Selected: ", selected_smoothing," (excluding mortality crises)")) +
                        theme(plot.subtitle = element_text(color = "red")) + 
                        personal_theme
        }
        return(gg)
}

# plots of abridged serie
plot_trends5 <- function(name, country_data5, x = "ex"){
        
        country_data5_plot <- country_data5 %>%
                mutate(Sex = factor(Sex, levels=c("m","f"), labels = c("Male","Female")),
                       Age_mean = (Age+Age+AgeInt)/2,
                       Age_mean = ifelse(Age==100, 100+ex,Age_mean),
                       Age_group = paste0(Age,"-",lead(Age)),
                       Age_group = ifelse(Age==100, "100+",Age_group),
                       Age_group = as.factor(Age_group))
        country_data5_plot$Age_group <- factor(country_data5_plot$Age_group,
                                               levels=
                                                       c("0-1",    "1-5"  ,  "10-15" , "15-20"  ,"20-25" , "25-30"  ,"30-35" , "35-40" ,
                                                         "40-45",  "45-50" , "5-10"   ,"50-55"  ,"55-60"  ,"60-65" , "65-70" , "70-75",  "75-80" ,
                                                         "80-85" , "85-90"  ,"90-95"  ,"95-100" ,"100+"))
        # my theme
        personal_theme <- theme(plot.title = element_text(hjust = 0.5),
                                text = element_text(size = 16),
                                plot.margin=grid::unit(c(0,0,0,0), "mm"),
                                plot.caption = element_text(color = 1, face = "italic", size=11))
        if(x=="mx5_time"){
                
                country_data5_plot <- country_data5_plot %>% 
                        mutate(label = ifelse(Date %in% 1950.5, as.character(Age_group),""))
                
                gg <- ggplot(data = country_data5_plot,
                             aes(x=Date,y=nMx,
                                 col=Age_group, label=label)) + 
                        geom_line() +
                        ggrepel::geom_text_repel(size=3, col="blue4",
                                                 seed = 42, box.padding = 0.5,
                                                 min.segment.length = 0, segment.color="red",max.overlaps = Inf,
                                                 point.size = NA) +
                        scale_y_log10(labels = comma)+
                        scale_colour_viridis_d(option = "B")+
                        labs(y=TeX("$log({}_nm_{x})$"), caption = Sys.time())+
                        theme_bw()+
                        theme(legend.position="none")+
                        labs(colour="Age", x="Year")+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        facet_grid(~Sex)+
                        ggtitle(paste0("Time trend of mortality rates by 5-age groups. ",name)) + 
                        # geom_text(data = to_label5,aes(x=Date,y=nMx,label = Age_group), color="blue", size=3) +
                        personal_theme
        }
        
        if(x=="mx5_age"){
                
                country_data5_plot <- country_data5_plot %>% 
                        mutate(label = ifelse(Date %in% seq(1950.5,2020.5,5) & Age==25, floor(Date),""))
                
                gg <- ggplot(data = country_data5_plot,
                             aes(x=Age_mean,y=nMx,
                                 col=Date, group=Date,label=label)) + 
                        geom_line() +
                        geom_point() +
                        ggrepel::geom_text_repel(size=3, col="blue4",
                                                 seed = 42, box.padding = 0.5,
                                                 min.segment.length = 0, segment.color="red",max.overlaps = Inf,
                                                 point.size = NA) +
                        scale_y_log10(labels = comma)+
                        scale_colour_viridis_c(option = "B")+
                        labs(y=TeX("$log({}_nm_{x})$"), caption = Sys.time())+
                        theme_bw()+
                        labs(colour="Year", x="Age")+
                        scale_x_continuous(breaks = seq(0,100,10),
                                           labels = seq(0,100,10))+
                        facet_grid(~Sex)+
                        ggtitle(paste0("Time trend of mortality rates by 5-age. ",name)) + 
                        # geom_text(data = to_label5,aes(x=Age_mean,y=nMx,label = Date), color="blue", size=3) +
                        personal_theme
        }
        
        if(x=="sex_ratio_time5"){
                
                data_sr <- country_data5_plot %>% select(Date,Age,Age_group, Sex, nMx) %>% 
                        pivot_wider(names_from = Sex, values_from = nMx) %>% 
                        mutate(rat = pmin(Male/Female,5),
                               label = ifelse(Date %in% 1950.5, as.character(Age_group),""))
                gg <- ggplot(data_sr, aes(x=as.numeric(Date),y=rat,
                                          col=Age_group, label=label)) + 
                        geom_line() +
                        ggrepel::geom_text_repel(size=3, col="blue4",
                                                 seed = 42, box.padding = 0.5,
                                                 min.segment.length = 0, segment.color="red",max.overlaps = Inf,
                                                 point.size = NA)+
                        geom_hline(yintercept = 1,linetype=2)+
                        scale_colour_viridis_d(option = "D")+
                        theme_bw()+
                        labs(colour="Age", x="Year",y="M/F ratio", caption = Sys.time())+
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        theme(legend.position="none")+
                        ggtitle(TeX(paste0("Time trend of sex ratio of mortality 5-age rates (male/female). ",name))) +
                        personal_theme
        }
        if(x=="sex_ratio_age5"){
                
                data_sr <- country_data5_plot %>% select(Date, Age,Age_mean, Sex, nMx) %>% 
                        pivot_wider(names_from = Sex, values_from = nMx) %>% 
                        mutate(rat = pmin(Male/Female,5),
                               label = ifelse(Date %in% seq(1950.5,2020.5,5) & Age==25, floor(Date),""))
                
                gg <-ggplot(data = data_sr,
                            aes(x=Age_mean,y=rat,
                                col=Date,group=Date,label=label)) + 
                        geom_line() +
                        geom_point() +
                        ggrepel::geom_text_repel(size=3, col="blue4",
                                                 seed = 42, box.padding = 0.5,
                                                 min.segment.length = 0, segment.color="blue4",max.overlaps = Inf,
                                                 point.size = NA)+
                        geom_hline(yintercept = 1,linetype=2)+
                        scale_colour_viridis_c(option = "A")+
                        theme_bw()+
                        labs(y="M/F ratio", color = "Year", caption = Sys.time())+
                        scale_x_continuous(name ="Age",
                                           breaks = seq(0,100,10), 
                                           labels = seq(0,100,10))+
                        ggtitle(paste0("Sex ratio (male/female) of mortality rates by 5-age groups with time. ",name)) +
                        personal_theme
        }
        return(gg)
}

        
plot_old_age <- function(name, country_data, post_old_age_adj, selected_data){       
                old_age <- bind_rows(
                        post_old_age_adj %>% select(Source,Age, Sex, Date, nMx, lx) %>% 
                                pivot_longer(cols=nMx:lx,names_to="Indicator", values_to="DataValue") %>% 
                                mutate(Adjusted = "Yes"),
                        country_data %>% select(Source=DataSourceShortName, Age = AgeStart, Sex=SexName, 
                                                Date=TimeMid, Indicator=IndicatorName,
                                                DataValue) %>% 
                                mutate(Indicator = case_when(Indicator=="l(x) - abridged"~"lx",
                                                             Indicator=="m(x,n) - abridged"~"nMx", 
                                                             TRUE~ "nada")) %>% 
                                mutate(Adjusted = "No")) %>% 
                        arrange(Source,Date,Sex,Adjusted,Age)  %>% 
                        left_join(selected_data %>% 
                                          select(Source=DataSourceShortName, Sex=SexName, 
                                                 Date=TimeMid, Indicator=IndicatorName) %>% 
                                          mutate(Indicator = case_when(Indicator=="l(x) - abridged"~"lx",
                                                                       Indicator=="m(x,n) - abridged"~"nMx", 
                                                                       TRUE~ "nada"),
                                                 selected = 1), 
                                  by=c("Source","Sex","Date","Indicator")) %>% 
                        filter(Age>=60,!Indicator %in% c("lx","nada"),selected==1) %>% 
                        mutate(Age_group = paste0(Age,"-",lead(Age)),
                               Age_group = ifelse(Age==100, "100+", Age_group),
                               Age_group = ifelse(Age>50 & lead(Age)!=(Age+5), paste0(Age,"+"),Age_group),
                               Age_group = as.factor(Age_group))
                
                personal_theme <- theme(plot.title = element_text(hjust = 0.5),
                                        text = element_text(size = 16),
                                        plot.margin=grid::unit(c(0,0,0,0), "mm"),
                                        plot.caption = element_text(color = 1, face = "italic", size=11))
                
                labels_age_groups <- as.character(unique(old_age$Age_group))
                old_age$Age_group <- factor(old_age$Age_group,
                                levels=c(labels_age_groups[labels_age_groups!="100+"],"100+"))
                
                gg <- ggplot(old_age %>% filter(!is.na(Age_group)),
                       aes(x=Date, y=DataValue, color=factor(Age_group)))+
                        geom_line(aes(linetype=factor(Adjusted)))+
                        scale_y_log10()+
                        scale_colour_viridis_d(option = "c")+
                        geom_point(aes(shape=Source))+
                        theme_bw()+
                        labs(x="Year",y="", caption = Sys.time(), color="Age", linetype="Fitted") +
                        scale_x_continuous(breaks = seq(1950,2020,10), 
                                           labels = seq(1950,2020,10))+
                        ggtitle(paste0("Old age mortality rates by 5-year groups fitted and not. ", name),
                                subtitle = "This is previous to any smoothing. Only for used data") +
                        facet_grid(rows = vars(Indicator), 
                                   cols = vars(Sex),
                                   scales = "free_y",space = "free",switch = "y")+
                        personal_theme
        return(gg)
}
