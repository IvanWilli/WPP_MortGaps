############## Belgium

# libraries
.packages = c("devtools", "data.table","DDSQLtools","DemoTools","jsonlite","openssl","tidyverse")
xfun::pkg_attach(.packages)
source("R/funs.R")

# countries
locs_tier1 <- read.csv("data/locs_tier1.csv") 

# country code and dates out
country_i = "Belgium"; dates_out = 1950.5:2020.5;

# get data
country_data <- read.csv(paste0("data/",country_i,".csv"))

# wich data is available
plot_data(country_data %>% filter(!IndicatorName %in% c("l(x) - abridged","l(x) - complete")))
country_data %>% count(DataSourceShortName,IndicatorName)
