
options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")

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
     # i = 1
     out <- get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),
                           startYear = 1950,
                           endYear = 2020,
                           indicatorIds = indicatorIds_extra,
                           locIds = locs_tier1[i],
                           locAreaTypeIds = 2,     ## "Whole area"
                           subGroupIds = 2,        ## "Total or All groups"
                           includeUncertainty = FALSE,
                           collapse_id_name = FALSE) %>% 
          as.data.table()
     lista_tier1[[j]] = out 
     print(j)
     j = j + 1
}

# save
save(lista_tier1, file = "data/data_tier1.Rdata")

# read
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
data_example <- get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),
                      startYear = 1980,
                      endYear = 2020,
                      indicatorIds = indicatorIds,
                      locIds = 32,
                      locAreaTypeIds = 2,     ## "Whole area"
                      subGroupIds = 2,        ## "Total or All groups"
                      includeUncertainty = FALSE,
                      collapse_id_name = FALSE) %>% 
        as.data.table() %>% 
        deduplicates(.)

data_example %>% names() %>% sort()
data_example %>% select(TimeLabel,IndicatorName,IndicatorID,id,SeriesID,SexName,AgeStart) %>% head(100)
data_example %>% count(DataSourceShortName, DataTypeName)




