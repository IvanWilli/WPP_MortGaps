############## tier 1

# to do
# - revisar no zeros - ok
# - jump-off year (Lee-Miller 2001) - check
# - replicate e0
# - cross-over (Li)
# - optimal period (Booth, 2002) 
# - documentar - ok
# - set 5 diagnostics: dx, sex-ratio, ... - ok
# - improve graphs - ok
# - make workflow - ok

# libraries 
xfun::pkg_attach(c("devtools", "data.table", "DemoTools","DDSQLtools","jsonlite","openssl",
                   "tidyverse","patchwork","gridExtra","tictoc","plotly"))
options(dplyr.summarise.inform = FALSE)
source("R/funs.R")
source("R/fill_gaps.R")

# countrys -----------------------------------------------------------------
locs_tier1 <- read.csv("data/locs_tier1.csv") 

# results -----------------------------------------------------------------
poss_fill_gaps_tier1 <- possibly(.f = fill_gaps_tier1, otherwise = "Error")
tic()
tier1_results <- 
     sample(locs_tier1$Name,3) %>% 
     split(list(.)) %>% 
     map(~poss_fill_gaps_tier1(country=.x, jump_off = FALSE))
toc()

# grafs - pdfs --------------------------------------------------------------------
# load("tier1_results.Rdata")
for(i in 1:3){
        # i = 1
        name <- names(tier1_results)[i]
        pdf(paste0("plots/",paste0(name),".pdf"))
        tier1_results_i <- tier1_results[[name]]
        print(tier1_results_i$plots)
        dev.off()
}

# tables ------------------------------------------------------------------
tier1_tables <- tier1_results %>% 
     lapply(function(X){
          browser()
          X$plots <- NULL
          X <- map(X, ~as.data.frame(.x))
     })

save(tier1_tables,file = "tier1_tables.Rdata")

