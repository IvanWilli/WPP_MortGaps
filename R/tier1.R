############## tier 1

# to do
# - jump off in lim lc - ok
# - revisar no zeros - ok
# - jump-off year (Lee-Miller 2001) - ok
# - replicate e0 - ok
# - cross-over (Li) - ok
# - optimal period (Booth, 2002) 
# - documentar - ok
# - set 5 diagnostics: dx, sex-ratio, ... - ok
# - improve graphs - ok

# libraries 
xfun::pkg_attach(c("devtools", "data.table", "DemoTools", "DemoToolsData", "DDSQLtools","jsonlite","openssl",
                   "tidyverse","patchwork","gridExtra","tictoc","plotly", "furrr","ggrepel"))
options(dplyr.summarise.inform = FALSE)
source("R/funs.R")
source("R/fill_gaps.R")
source("C:/Proyectos/DemoTools/R/interp_lc_lim.R")

# countrys -----------------------------------------------------------------
locs_tier1 <- read.csv("data/locs_tier1.csv") 

# results -----------------------------------------------------------------
# casos <- locs_tier1$Name[sample(1:42,2)]
casos <- locs_tier1$Name
casos <- c("Bosnia and Herzegovina","Croatia","Republic of Moldova","North Macedonia",
                "Slovenia","Albania","Montenegro","Greece","Latvia","Ukraine","Lithuania","Serbia")
casos <- c("Estonia","Greece","Croatia")

poss_fill_gaps_tier1 <- possibly(.f = fill_gaps_tier1, otherwise = "Error")
plan(strategy = multiprocess)
tic()
tier1_results <- 
        casos_error %>%
        split(list(.)) %>% 
        future_map(~poss_fill_gaps_tier1(country=.x,
                prev_cross_over = TRUE, jump_off = FALSE, LC_fit_e0 = TRUE))
toc()
save(tier1_results,file = "data/tier1_results2_adj.Rdata")

# grafs - pdfs --------------------------------------------------------------------
tier1_results2$Belarus <- NULL
for(i in names(tier1_results2)){
        # i = 1
        pdf(paste0("plots/",paste0(i),"_no_adj.pdf"))
        tier1_results_i <- tier1_results2[[i]]
        print(tier1_results_i$plots)
        dev.off()
}

# errors in data ----------------------------------------------------------
error_tables <- tier1_results2_adj %>% 
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

# export
write.xlsx(error_summary %>% as.data.frame(),file = "data/error_summary.xlsx",row.names = F,sheetName = "Summary")
write.xlsx(error_details %>% as.data.frame(),file = "data/error_summary.xlsx",row.names = F,sheetName = "Details",append = T)

# diagn gaps --------------------------------------------------------------
tier1_results2$`Republic of Moldova` <- NULL
gaps_rank <- bind_rows(
        tier1_results2 %>% 
                map(~pluck(.,"tables")) %>% 
                map(~pluck(.,"gaps")) %>% 
                map_df(~length(unlist(.))),
        tier1_results2 %>% 
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
