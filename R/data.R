library(tidyverse)
library(DDSQLtools)
library(data.table)
options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")
.packages = c("devtools", "data.table","DDSQLtools","DemoTools","jsonlite","openssl","tidyverse")
lapply(.packages, require, character.only=TRUE)
source("R/funs.R")

# get locs
tier1 <- readxl::read_xlsx("data/Country_availability_summary.xlsx",
                           sheet = "summary",range = "A5:A48",col_names = F) %>% 
                         as.data.frame() %>% pull()
locs <- get_locations()
locs_tier1 <- locs %>% filter(Name %in% tier1) %>% pull(PK_LocID)
all_Indicadors <- get_iitypes() %>% 
        filter(IndicatorType.ComponentName %in% c("Mortality","Life tables")) %>% 
        select(PK_IndicatorID, Name, IndicatorType.ComponentName)
indicatorIds = c(255,256,234,239,272,245,246)
indicatorIds_extra = c(indicatorIds,
                       229,253,273,274)
indicatorIds_extra = c(indicatorIds,219,220)

# go server
lista_tier1 <- list()
j = 1
for(i in seq_along(locs_tier1)){
     out <- get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),
                           startYear = 1940,
                           endYear = 2020,
                           indicatorIds = indicatorIds_extra,
                           locIds = locs_tier1[i],
                           locAreaTypeIds = 2,     ## "Whole area"
                           subGroupIds = 2,        ## "Total or All groups"
                           includeUncertainty = FALSE,
                           collapse_id_name = FALSE) %>% 
          as.data.table() %>% deduplicates(.)
     lista_tier1[[j]] = out 
     print(j)
     j = j + 1
}

# save
# save(lista_tier1, file = "data/data_tier1.Rdata")

# read
load("data/data_tier1.Rdata")
# load("data/data2_tier1.Rdata")

# set a csv for each
for(i in 1:length(lista_tier1)){
     name <- unique(lista_tier1[[i]]$LocName)
     write.csv(lista_tier1[[i]],paste0("data/",name,".csv"), row.names = F)
}
for(i in 1:length(lista2_tier1)){
     name <- unique(lista2_tier1[[i]]$LocName)
     write.csv(lista2_tier1[[i]],paste0("data/",name,".csv"), row.names = F)
}


# specific country --------------------------------------------------------
indicatorIds_extra = c(indicatorIds,
                       219,220,229,253,273,274)
Albania_data <- get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),
                      startYear = 1940,
                      endYear = 2020,
                      indicatorIds = indicatorIds_extra,
                      locIds = 8,
                      locAreaTypeIds = 2,     ## "Whole area"
                      subGroupIds = 2,        ## "Total or All groups"
                      includeUncertainty = FALSE,
                      collapse_id_name = FALSE) %>% 
        as.data.table() %>% 
        deduplicates(.)


# HMD plot ----------------------------------------------------------------
files <- c(list.files(path = "data/HMD/fltper_1x1", full.names = T),
           list.files(path = "data/HMD/mltper_1x1", full.names = T))
HMD_data <- map(files, function(X) {read.table(X, skip = 2, header = T) %>% 
                                        mutate(Country = X)}) %>% 
                do.call(rbind,.) %>% 
                filter(Year >= 1950) %>% 
                mutate(Sex = substr(Country,10,10), 
                       Country = substr(Country,21,23),
                       ex = as.numeric(ex),
                       lx = as.numeric(lx))

g_HMD_ex <- ggplot(HMD_data %>% filter(Age %in% c(0,80)))+
        geom_point(aes(x=Year,y=ex), color = "grey", alpha=.5)+
        theme_bw()+
        labs(x="Year",y="e(x)") +
        facet_grid(rows = vars(Age), 
                   cols = vars(Sex),
                   scales = "free_y")

g_HMD_q <- ggplot(HMD_data %>%
                group_by(Year,Sex,Country) %>% 
                summarise(`0q5` = 1-lx[Age==5]/lx[Age==0],
                         `60q20` = 1-lx[Age==80]/lx[Age==20]))+
                geom_point(aes(x=`0q5`,y=`60q20`), color = "grey", alpha=.5)+
                theme_bw()+
                scale_x_log10()+scale_y_log10()+
                labs(x="0q5",y="60q20") +
                facet_grid(cols = vars(Sex),
                           scales = "free_y")
save(g_HMD_ex,g_HMD_q,file = "data/HMD/g_HMD.Rdata")
