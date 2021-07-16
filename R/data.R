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
lista2_tier1 <- list()
all_Indicadors <- get_iitypes() %>% 
        filter(IndicatorType.ComponentName %in% c("Mortality","Life tables")) %>% 
        select(PK_IndicatorID, Name, IndicatorType.ComponentName)

indicatorIds = c(255,256,234,239,272,245,246)

# go server
j = 1
for(i in 30:length(locs_tier1)){
     out <- get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),
                           startYear = 1940,
                           endYear = 2020,
                           indicatorIds = indicatorIds,
                           locIds = locs_tier1[i],
                           locAreaTypeIds = 2,     ## "Whole area"
                           subGroupIds = 2,        ## "Total or All groups"
                           includeUncertainty = FALSE,
                           collapse_id_name = FALSE) %>% 
          as.data.table() %>% deduplicates(.)
     lista2_tier1[[j]] = out 
     j = j + 1
}

# save
# save(lista2_tier1, file = "data/data2_tier1.Rdata")
# save(lista_tier1, file = "data/data_tier1.Rdata")

# read
load("data/data_tier1.Rdata")
load("data/data2_tier1.Rdata")

# set a csv for each
for(i in 1:length(lista_tier1)){
     name <- unique(lista_tier1[[i]]$LocName)
     write.csv(lista_tier1[[i]],paste0("data/",name,".csv"), row.names = F)
}
for(i in 1:length(lista2_tier1)){
     name <- unique(lista2_tier1[[i]]$LocName)
     write.csv(lista2_tier1[[i]],paste0("data/",name,".csv"), row.names = F)
}

### specific country - extra data
indicatorIds_extra = c(255,256,234,239,272,245,246,
                       219,220,229,253)
out <- get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),
                      startYear = 1940,
                      endYear = 2020,
                      indicatorIds = indicatorIds_extra,
                      locIds = 498,
                      locAreaTypeIds = 2,     ## "Whole area"
                      subGroupIds = 2,        ## "Total or All groups"
                      includeUncertainty = FALSE,
                      collapse_id_name = FALSE) %>% 
        as.data.table() %>% deduplicates(.)
