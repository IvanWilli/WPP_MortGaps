############## Greece

# libraries
.packages = c("devtools", "data.table","DDSQLtools","DemoTools","jsonlite","openssl","tidyverse")
xfun::pkg_attach(.packages)
source("R/funs.R")

# countries
locs_tier1 <- read.csv("data/locs_tier1.csv") 

# country code and dates out
country_i = "Greece"; dates_out = 1950.5:2020.5;

# get data
country_data <- read.csv(paste0("data/",country_i,".csv"))

# wich data is available
plot_data(country_data %>% 
               filter(!IndicatorName %in% c("l(x) - abridged","l(x) - complete")))
country_data %>% count(DataSourceShortName,IndicatorName)

# data
     # 1) HMD 1980-2017
     # 2) EuroStat 1960-1980
     # 3) HLD: complete 1957 and 1940, abridged 1950
# strategy: mix 1 and 2 or use EuroStat? 

# select best data: single & HMD
m_single <- country_data %>% filter(IndicatorName=="m(x,n) - complete",
                                    SexName!="Both sexes")

# ages of lt
m_single %>% 
     group_by(DataSourceShortName,TimeLabel) %>% 
     summarise(OAG=max(AgeStart))  %>% 
     pivot_wider(names_from="DataSourceShortName",values_from="OAG") %>% 
     arrange(TimeLabel) %>% 
     as.data.frame()

# same data?
m_single %>% filter(TimeMid==1990.5,AgeStart%in%c(0,5,50,80)) %>% 
     select(DataSourceShortName,DataValue,AgeStart,SexName) %>% 
     pivot_wider(names_from="DataSourceShortName",values_from="DataValue")