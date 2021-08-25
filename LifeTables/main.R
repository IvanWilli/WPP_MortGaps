# author: IW
# receives parameters from batch, upload libraries that are needed, detect where to read InputFiles and
# run the `fill_gaps` functions for building life table time series.

rm(list = objects())

# receive arguments --------------------------------------------------------------------

args <- commandArgs(trailing = TRUE)

root_dir       <- args[1]             ## args[1] ## "D:"
WPP_revision   <- as.numeric(args[2]) ## args[2] ## 2021	
WPP_RevID      <- as.numeric(args[3]) ## args[3] ## used for shortnotes
plots_dir      <- args[4]             ## "c:/WPP2021_plots"
myLocID        <- as.numeric(args[5])
isVerbose      <- args[6] ## FALSE
isLocal        <- args[7] ## FALSE

WPP_revision   <- 2021
WPP_RevID      <- 19
myLocID        <- 376
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

# List of packages for session: needs manually MortalitySmooth (or https://github.com/timriffe/MortalitySmooth)
.packages = c("devtools", "data.table", "DemoTools", "DemoToolsData", "DDSQLtools","jsonlite","openssl",
              "tidyverse","patchwork","gridExtra","tictoc","plotly", "furrr","ggrepel","fertestr",
              "colorspace", "scales","latex2exp","readxl","openxlsx","logr","ungroup","MortalitySmooth","StMoMo")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],dependencies=TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

## openxlsx converts columns of class "POSIxt" to Excel datetimes with the format given as
options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
options(scipen = 9999)
options(dplyr.summarise.inform = FALSE)
Sys.setenv(TZ = "America/New_York")

# directories and source funtions to use -------------------------------------------------------------

# on root
if(isLocal){
        root_dir <- "C:/Proyectos/WPP_MortGaps"
}
setwd(root_dir)

# set directoires related to root
LT_dir         <- file.path(root_dir, "LifeTables")
plots_dir      <- file.path(root_dir, "WPP2021_plots")
global_dir     <- file.path(root_dir, "GlobalFiles")

# funs
source(file.path(LT_dir,"fill_gaps_lt.R"))
source(file.path(LT_dir,"fill_gaps.R"))
source(file.path(LT_dir,"funs.R"))
source(file.path(LT_dir,"plot_funs.R"))
options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")

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
         
# run a country
# debugonce(fill_gaps)
fill_gaps(country = country_full, 
          dirs = data.frame(LT_dir=LT_dir, plots_dir=plots_dir, global_dir=global_dir, InputFiles_dir=InputFiles_dir),
          dates_out = 1950.5:2020.5,
          OAnew = 100,
          first_year = 1950,
          last_year = WPP_revision-1)

# close log
log_close()


# End ---------------------------------------------------------------------


