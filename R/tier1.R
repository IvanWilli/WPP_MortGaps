
########### run tier1 countries. Work in progress

# load --------------------------------------------------------------------
# libraries 
xfun::pkg_attach(c("devtools", "data.table", "DemoTools", "DemoToolsData", "DDSQLtools","jsonlite","openssl",
                   "tidyverse","patchwork","gridExtra","tictoc","plotly", "furrr","ggrepel","fertestr",
                   "colorspace", "scales"))
options(dplyr.summarise.inform = FALSE)
# funs
source("R/funs.R")
source("R/fill_gaps.R")
source("R/plot_funs.R")
source("C:/Proyectos/DemoTools/R/interp_lc_lim.R")

# countrys -----------------------------------------------------------------
locs_tier1 <- read.csv("data/locs_tier1.csv") 

# apply -----------------------------------------------------------------
casos <- locs_tier1$Name[1:20]
# function for looping in paralell
poss_fill_gaps_tier1 <- possibly(.f = fill_gaps_tier1, otherwise = "Error")
plan(strategy = multiprocess)
tic()
tier1_results <-
        casos %>%
        split(list(.)) %>%
        future_map(~poss_fill_gaps_tier1(country=.x,
                prev_cross_over = TRUE, jump_off = FALSE, LC_fit_e0 = TRUE))
toc()
# save results
save(tier1_results,file = "data/tier1_results.Rdata")

# do grafs --------------------------------------------------------------------
load("data/tier1_results1.Rdata")
load("data/HMD_data.Rdata")
HMD_data <- HMD_data %>% 
        filter(SexName != "Both sexes") %>% 
        mutate(Sex = factor(SexName, levels = c("Male","Female"))) 
        # mutate(Sex = ifelse(SexName=="Female","f","m"))
for(i in names(tier1_results)){
        # i = "Sweden"
        name_code = tier1_results[[i]]$name$Code_iso
        name = tier1_results[[i]]$name$Name
        t_final_data = tier1_results[[i]]$final_data
        country_data = tier1_results[[i]]$country_data
        selected_data = tier1_results[[i]]$selected_data
        pdf(paste0("plots/",name_code,"_LT1_XEmpirical-Data-Sources.pdf"))
        print(plot_data_selected(name, country_data, selected_data))
        dev.off()
        pdf(paste0("plots/",name_code,"_ex_XLife-expectancy-Estimates-Time-Trend-comparison.pdf"))
        print(plot_comparison(name, t_final_data, HMD_data, x = "e0&e80"))
        dev.off()
        pdf(paste0("plots/",name_code,"_1q0_XInfant-mortality-Estimates-Time-Trend-comparison.pdf"))
        print(plot_comparison(name, t_final_data, HMD_data,x = "1q0"))
        dev.off()
        pdf(paste0("plots/",name_code,"_5q0_XUnder-five-mortality-Estimates-Time-Trend-comparison.pdf"))
        print(plot_comparison(name, t_final_data, HMD_data,x = "5q0"))
        dev.off()
        pdf(paste0("plots/",name_code,"_4q1_XChild-mortality-Estimates-vs-1q0-Time-Trend-comparison.pdf"))
        print(plot_comparison(name, t_final_data, HMD_data,x = "1q0vs4q1"))
        dev.off()
        pdf(paste0("plots/",name_code,"_45q15_XAdult-mortality-Estimates-Time-Trend-comparison.pdf"))
        print(plot_comparison(name, t_final_data, HMD_data,x = "45q15"))
        dev.off()
        pdf(paste0("plots/",name_code,"_45q15_XAdult-mortality-Estimates-vs-5q0-Time-Trend-comparison.pdf"))
        print(plot_comparison(name, t_final_data, HMD_data,x = "45q15vs5q0"))
        dev.off()
        pdf(paste0("plots/",name_code,"_e60_XOld-age-mortality-Estimates-vs-45q15-Time-Trend-comparison.pdf"))
        print(plot_comparison(name, t_final_data, HMD_data,x = "e60vs45q15"))
        dev.off()
        pdf(paste0("plots/",name_code,"_e80_XOld-age-mortality-Estimates-vs-45q15-Time-Trend-comparison.pdf"))
        print(plot_comparison(name, t_final_data, HMD_data,x = "e80vs45q15"))
        dev.off()
        pdf(paste0("plots/",name_code,"_ex_XLife-expectancy-Estimates-Time-Trend.pdf"))
        print(plot_trends(name, t_final_data,x="ex"))
        dev.off()
        pdf(paste0("plots/",name_code,"_Mx1_XEstimates-Time-Trend.pdf"))
        print(plot_trends(name, t_final_data,x="mx_time"))
        dev.off()
        pdf(paste0("plots/",name_code,"_Mx1_XEstimates-Age-Profiles.pdf"))
        print(plot_trends(name, t_final_data,x="mx_age"))
        dev.off()
        pdf(paste0("plots/",name_code,"_Mx1_XEstimates-Lexis-surface.pdf"))
        print(plot_trends(name, t_final_data,x="lexis"))
        dev.off()
        pdf(paste0("plots/",name_code,"_Mx1_XEstimates-Sex-Ratios-Time-Trend.pdf"))
        print(plot_trends(name, t_final_data,x="sex_ratio_time"))
        dev.off()
        pdf(paste0("plots/",name_code,"_Mx1_XEstimates-Sex-Ratios-Age-Profiles.pdf"))
        print(plot_trends(name, t_final_data,x="sex_ratio_age"))
        dev.off()
        pdf(paste0("plots/",name_code,"_Mx1_XEstimates-Sex-Ratios-Lexis-surface.pdf"))
        print(plot_trends(name, t_final_data,x="sex_ratio_lexis"))
        dev.off()
}
        
# errors in data ----------------------------------------------------------
error_tables <- tier1_results %>% 
        map(~pluck(.,"tables")) %>%
        keep(~length(.x)>0) %>% 
        map(~pluck(.,"error_data")) %>% 
        keep(~nrow(.x)>0) %>% 
        map(~ .x %>% mutate_at(vars(coherence), ~as.character(.))) %>% 
        bind_rows(.id = "Country") %>% 
        distinct_at(vars(Country,DataSourceShortName,TimeLabel,IndicatorName,SexName))
error_summary <- error_tables %>% 
        mutate(Error = "x") %>% 
        pivot_wider(names_from=DataSourceShortName,
                    values_from=Error)
# get original data with errors
error_details <- casos %>% 
        map(~read.csv(paste0("data/",.x,".csv"))) %>% 
        do.call(rbind,.) %>%
        mutate(SexName = ifelse(SexName=="Female","f","m")) %>% 
        inner_join(error_tables %>% rename(LocName=Country),
                   by=c("LocName", "IndicatorName","TimeLabel","DataSourceShortName","SexName"))

# diagn gaps --------------------------------------------------------------
gaps_rank <- bind_rows(
        tier1_results %>% 
                map(~pluck(.,"tables")) %>% 
                map(~pluck(.,"gaps")) %>% 
                map_df(~length(unlist(.))),
        tier1_results %>% 
                map(~pluck(.,"tables")) %>% 
                map(~pluck(.,"gaps")) %>% 
                map_df(~length(.))) %>% 
        mutate(ranking = c("Years","Gaps")) %>% 
        pivot_longer(!ranking, names_to = "Country",values_to = "value") 

g_gap_clusters <- ggplot(gaps_rank %>% 
                        pivot_wider(names_from="ranking",values_from="value"),
                        aes(Gaps,Years)) +
                geom_point(size=2)+
                labs(y="Total lenght of years on gaps", x="Priods with gaps") +
                geom_text_repel(aes(label = Country),col="grey") +
                scale_x_continuous(labels = c("1 gap","2 gaps","3 gaps"), breaks = 1:3)+
                ggtitle("Countries by length and period of data gaps.") +
                theme_bw() 
pdf("plots/clusters_tier1.pdf")
print(g_gap_clusters)
dev.off()

# casos <- c("Bosnia and Herzegovina","Croatia","Republic of Moldova","North Macedonia",
#            "Slovenia","Albania","Montenegro","Greece","Latvia","Ukraine","Lithuania","Serbia")

