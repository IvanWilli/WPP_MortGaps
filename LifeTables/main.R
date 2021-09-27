# author: IW
# receives parameters from batch, upload libraries that are needed, detect where to read InputFiles and
# run the `fill_gaps` functions for building life table time series.

rm(list = objects())

# receive arguments --------------------------------------------------------------------

# from batch
args <- commandArgs(trailing = TRUE)
root_dir       <- args[1]             ## args[1] ## "D:"
WPP_revision   <- as.numeric(args[2]) ## args[2] ## 2021	
WPP_RevID      <- as.numeric(args[3]) ## args[3] ## used for shortnotes
plots_dir      <- args[4]             ## "c:/WPP2021_plots"
myLocID        <- as.numeric(args[5])
isVerbose      <- args[6] ## FALSE
isLocal        <- args[7] ## FALSE

# or set it manually
WPP_revision   <- 2021
WPP_RevID      <- 19
myLocID        <- 276
isVerbose      <- FALSE
isLocal        <- TRUE
isVerbose <- as.logical(isVerbose)
isLocal   <- as.logical(isLocal)

if (isVerbose==FALSE) {
        options(warn=-1)
        options(verbose=FALSE)
        options(show.error.messages=TRUE)
} else {
        options(warn=0)
        options(verbose=TRUE)
        options(show.error.messages=TRUE)
}

# libraries ---------------------------------------------------------------

## use openxlsx 4.2.3 (version openxlsx 4.2.4 has some bug with formatting issues that loose the default formatting of column headers)
if (packageVersion('openxlsx') > '4.2.3'){
        unload("openxlsx")
        require(devtools)
        install_version("openxlsx", version='4.2.3', repos = "http://cran.us.r-project.org")
}

# List of packages for session. IMPORTANT: MortalitySmooth needs manually download from archive (or https://github.com/timriffe/MortalitySmooth)
.packages = c("devtools", "data.table", "DemoTools", "DemoToolsData", "DDSQLtools","jsonlite","openssl",
              "tidyverse","patchwork","gridExtra","tictoc","plotly", "furrr","ggrepel","fertestr", "zoo",
              "colorspace", "scales","latex2exp","readxl","openxlsx","logr","ungroup","MortalitySmooth",
              "StMoMo","MortCast","slider")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],dependencies=TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

## openxlsx converts columns of class "POSIxt" to Excel datetimes with the format given as
options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
options(scipen = 9999)
options(dplyr.summarise.inform = FALSE)
options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")
Sys.setenv(TZ = "America/New_York")

# directories and source funtions to use -------------------------------------------------------------

# set your root if is local
if(isLocal){
        root_dir <- "C:/Proyectos/WPP_MortGaps"
}
setwd(root_dir)

# set directoires related to root
LT_dir         <- file.path(root_dir, "LifeTables")
plots_dir      <- file.path(root_dir, "WPP2021_plots")
aux_dir     <- file.path(LT_dir, "AuxFiles")

# funs for empirical lt
source(file.path(LT_dir,"fill_gaps_lt.R"))
source(file.path(LT_dir,"fill_gaps.R"))
source(file.path(LT_dir,"funs.R"))
source(file.path(LT_dir,"plot_funs.R"))
# funs for model lt (form Sarah code)
source(file.path(LT_dir,"MLT/lt_model_cdun_match_single.R"))
source(file.path(LT_dir,"MLT/lt_model_cdun_combin_single.R"))
source(file.path(LT_dir,"MLT/lt_model_un_bestft.R"))
source(file.path(LT_dir,"MLT/WPP_MLT_mortality_patterns.R"))

myLocID_tier1 <- read.csv("data/locs_tier1.csv")$Name 
myLocID_celade <- c(32,68,76,152,170,188,192,214,218,222,320,332,340,484,558,591,600,604,858,862)
myLocID_iw <- c(32,52,312,28,533,44,531,308,388,662,780,850,84,226,24,678,254,328,740,132,624)
myLocID_danan <- c(344,158,196,702,144,156,446,792,418,764,96,360,458,608,462,64,496,104,626)
myLocID_thomas <- c(233,404,800,410,854,108,120,140,148,178,384,408,232,266,270,288,
                    324,430,454,466,562,566,646,686,694,706,768,204,180)
myLocID_giulia <- c(480,670,690,258,474,540,242,296,583,598,882,90,776,548,316,630,262,450,638)
myLocID_sarah <- c(4,48,50,356,364,368,400,414,422,524,512,586,634,682,275,760,784,887)
myLocID_vladimira <- c(268,51,31,398,417,762,795,860,12,818,434,478,504,728,729,788,732,174,175)
myLocID_patrick <- c(834,652,663,16,20,660,60,535,72,92,136,184,212,748,238,234,
                        292,304,336,833,426,438,584,492,500,508,516,520,570,580,585,654,
                        659,666,674,534,710,772,796,798,876,894,716,704,116)
myLocID_cases <- c(340, 222, 218, 792, 356)


for(myLocID in 70){
# myLocID = 858
print(myLocID)
        
# read InputFiles. IMPORTANT: check InputFiles url in case is not local
country_full <- get_ID_Name(myLocID)
if (isLocal){
        InputFiles_dir <- file.path("InputFiles", paste0(country_full$Code_iso, "_R", 21, ".xlsx"))
}else{
        eagle_URL       <- "https://popdiv.dfs.un.org/peps/eagle/api/file/ProcessedListCompact/"
        eagle_locations <- data.table(fromJSON(paste0(eagle_URL, 2021), flatten=TRUE))
        InputFiles_dir <- enc2native(eagle_locations[LocID==myLocID, PathInputFile])   
}

# run the country ---------------------------------------------------------

# Open log
log_open(file_name = file.path(LT_dir, paste0(myLocID, ".log")))

tic()
# run a country
fill_gaps(country = country_full, 
          dirs = data.frame(LT_dir=LT_dir, plots_dir=plots_dir, aux_dir=aux_dir, InputFiles_dir=InputFiles_dir),
          dates_out = 1950.5:2020.5,
          OAnew = 100,
          first_year = 1950,
          last_year = WPP_revision-1)
toc()

# close log
log_close()
}
# country = country_full
# dirs = data.frame(LT_dir=LT_dir, plots_dir=plots_dir, aux_dir=aux_dir, InputFiles_dir=InputFiles_dir)
# dates_out = 1950.5:2020.5
# OAnew = 100
# first_year = 1950
# last_year = WPP_revision-1


# End ---------------------------------------------------------------------