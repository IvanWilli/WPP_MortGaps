################ from Patrick Gerland code (2/7/2021)

rm (list = ls())

## R wrapper to connect to DemoData (and optionally to DemoTools library)
## https://timriffe.github.io/DDSQLtools/
## https://timriffe.github.io/DDSQLtools/articles/Downloading-UNPD-data-into-R.html
## (required) Tools for aggregate demographic analysis
## https://timriffe.github.io/DemoTools/

## Open API documentation for DemoData:
## https://popdiv.dfs.un.org//Demodata/swagger/ui/index#

## -----------------------------------------------------------------------
## install DemoTools and DDSQLtools (comments if already installed)
## devtools::install_github("timriffe/DemoTools", force=TRUE)
## devtools::install_github("timriffe/DDSQLTools", force=TRUE)
## -----------------------------------------------------------------------

# List of packages for session
.packages = c("devtools", "data.table","tictoc","dplyr","jsonlite","openssl","stringr", "ggplot2")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],dependencies=TRUE)

# Load packages into session
lapply(.packages, require, character.only=TRUE)


## devtools::install_github("timriffe/DDSQLTools", force=TRUE)
library(DDSQLtools)

# setwd("d:\\United Nations\\DESA-POP - DemoData\\LifeTable\\")

options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")

## get_indicators():     Get information about available indicators (IndicatorID)
## get_iitypes():     Get information about available indicators (IndicatorID) and indicatortypeids (IndicatorTypeId)
indicators <- data.table(get_iitypes())

indicators <- indicators[, .(IndicatorID=PK_IndicatorID, IndicatorName=Name, IndicatorShortName=ShortName, UnitShortLabel, VariableType, FormatString, ComponentID, ComponentName=IndicatorType.ComponentName, IndicatorTypeID, IndicatorTypeName=IndicatorType.Name, IsComplete, SortOrder)]
setorder(indicators, SortOrder)

unique(indicators$ComponentName)
indicators[ComponentName=="Population"]
indicators[ComponentName=="Fertility"]
indicators[ComponentName=="Mortality"]
indicators[ComponentName=="Life tables"]


## get_indicatortypes(): Get information about available indicators (IndicatorTypeID)

## get list of DataProcess
DataProcessType <- data.table(get_dataprocesstype())
DataProcess <- data.table(get_dataprocess())

DataProcess <- merge(DataProcess[, .(DataProcessID=PK_DataProcessID, DataProcessTypeID, Name, ShortName, SortOrder1)],
                     DataProcessType[, .(PK_DataProcessTypeID, DataProcessTypeName=Name, DataProcessTypShortNamee=ShortName)],
                     by.x="DataProcessTypeID", by.y="PK_DataProcessTypeID")

## example of selection for DataProcessTypeID
## 2=Census ; 11=Survey ; 12=Panel ; 8=PES
## 9=Register ; 7=Life Table (legacy UN DYB) ; 10=Sample Registration System (SRS)
## 6=Estimate
DataProcess[DataProcessTypeID %in% c(6, 7, 9, 10), ]



## get list of locations from Server
Locations <- data.table(get_locations(addDefault = "false",
                  includeDependencies = "false",
                  includeFormerCountries = "false"))
Locations <- Locations[, .(LocID=PK_LocID, LocTypeID, LocName=Name)]

DataCatalog <- data.table(get_datacatalog(addDefault = "false"))
DataProcess <- data.table(get_dataprocess(addDefault = "false"))
DataProcessType <- data.table(get_dataprocesstype(addDefault = "false"))


## get list of all DataSources
all_ds <- data.table(get_datasources())
dyb_sd <- data.table(get_datasources(shortNames = "DYB"))
ipums_sd <- data.table(get_datasources(shortNames = "IPUMS"))

myDS <- "DYB"
myDS <- "IPUMS"
## get all sources
myDS <- NULL


## de-duplicate function to rank and filter series based on different set of criteria
deduplicates <- function(myDT) {

	## myDT <- mx5_DB

	## Filter out Model-based estimates from IDB and GBD
	## but keep "model-based estimates" from VR computed for WPP using time interpolation to fill-in data gaps
	myDT <- myDT[(! DataTypeID %in% c(70, 71)) | (DataTypeID %in% c(70) & DataSourceShortName=="WPP")]
	## for HMD, keep only annual series
	myDT <- myDT[(DataSourceShortName!="HMD") | (DataSourceShortName=="HMD" & TimeDuration==1)]

	## sort records per location and year to order multiple observation by multi-criteria using sort orders
	setorder(myDT, LocID, TimeMid, DataCatalogShortName,
	StatisticalConceptSort,
	DataStatusSort,
	DataProcessSort, DataProcessTypeSort,
	DataReliabilitySort,
	-DataSourceYear, DataSourceSort, DataSourceShortName,
	-DataTypeSort,
	ModelPatternName, PeriodGroupName, PeriodStart, PeriodSpan,
	SexSort, AgeStart, AgeSpan)

	## subset key attributes to rank most authoritative series
	## mySeries <- unique(myDT[, .(SeriesID, LocID, DataCatalogShortName, TimeMid, DataSourceShortName, DataSourceYear, DataSourceSort, DataStatusName, DataStatusSort, DataProcessSort, DataProcessTypeSort, StatisticalConceptName, StatisticalConceptSort, DataTypeName, DataTypeSort, DataReliabilityName, DataReliabilitySort)])
	myDT[, MD5 := md5(paste(LocID, DataCatalogShortName, TimeMid, DataSourceShortName, DataSourceYear, DataSourceSort, DataStatusName, DataStatusSort, DataProcessSort, DataProcessTypeSort, StatisticalConceptName, StatisticalConceptSort, DataTypeName, DataTypeSort, DataReliabilityName, DataReliabilitySort, sep="-"))]
	mySeries <- unique(myDT[, .(MD5, LocID, DataCatalogShortName, TimeMid, DataSourceShortName, DataSourceYear, DataSourceSort, DataStatusName, DataStatusSort, DataProcessSort, DataProcessTypeSort, StatisticalConceptName, StatisticalConceptSort, DataTypeName, DataTypeSort, DataReliabilityName, DataReliabilitySort)])

	setorder(mySeries, LocID, DataCatalogShortName,
	StatisticalConceptSort,
	DataStatusSort,
	DataProcessSort, DataProcessTypeSort,
	DataReliabilitySort,
	-DataSourceYear, DataSourceSort, DataSourceShortName,
	-DataTypeSort)

	## assign rank to each set of "dups" (from different sources, etc.)
	mySeries[, nrank1 := 1:.N, by=list(LocID, DataCatalogShortName, DataProcessSort, DataTypeSort, trunc(TimeMid))]
	## for estimates and VR
	mySeries[, nrank2 := 1:.N, by=list(LocID, DataCatalogShortName, DataProcessSort, trunc(TimeMid))]
	## in case only 1 series get used
	mySeries[, nrank3 := 1:.N, by=list(LocID, DataCatalogShortName, trunc(TimeMid))]

	## keep only most authoritative versions (top #1) based on DataCatalogShortName, DataProcessSort, DataTypeSort
	myDT <- merge(myDT, mySeries[, .(MD5, nrank1, nrank2, nrank3)], by=c("MD5"))
	## alternative use nrank3 instead of nrank2 if we want to always pick Estimates > Register when both are available
	myDT <- myDT[(!DataProcessTypeID %in% c(6, 9) & nrank1==1) | (DataProcessTypeID %in% c(6, 9) & nrank2==1)]

	# data checking to verify which series get selected
	# myDT[SexID==1 & AgeStart==0 & TimeMid >= 1950 & TimeMid <= 1951]
	# myDT[SexID==1 & AgeStart==0 & TimeMid >= 1960 & TimeMid <= 1961]
	# myDT[SexID==1 & AgeStart==0 & TimeMid >= 1970 & TimeMid <= 1971]
	# myDT[SexID==1 & AgeStart==0 & TimeMid >= 1980 & TimeMid <= 1981]
	# myDT[SexID==1 & AgeStart==0 & TimeMid >= 1990 & TimeMid <= 1991]
	# myDT[SexID==1 & AgeStart==0 & TimeMid >= 2000 & TimeMid <= 2001]
	# myDT[SexID==1 & AgeStart==0 & TimeMid >= 2010 & TimeMid <= 2011]

	myDT$MD5 <- NULL
	myDT$nrank1 <- NULL
	myDT$nrank2 <- NULL

	return(myDT)
}


## query EAGLE list of locations
WPP_revision   <- 2021
eagle_URL      <- "https://popdiv.dfs.un.org/peps/eagle/api/file/ProcessedListCompact/"
eagle_locations <- data.table(fromJSON(paste0(eagle_URL, WPP_revision), flatten=TRUE))

## example of test using Senegal, either using LocID or name
## myLocations <- c(686)
## myLocations <- "Senegal"

myLocations <- sort(unique(Locations$LocID))

## for initial focus filter out smaller locations
myLocations <- eagle_locations[IsSmall==FALSE, LocID]

## LocID HMD LT1
## 276	1956-2017	Germany
## 616	1958-2019	Poland
## 804	1959-2013	Ukraine
## 643	1959-2014	Russia
## 112	1959-2018	Belarus
## 233	1959-2019	Estonia
## 428	1959-2019	Latvia
## 440	1959-2019	Lithuania
## 442	1960-2019	Luxembourg
## 300	1981-2017	Greece
## 705	1983-2017	Slovenia
## 152	1992-2017	Chile
## 191	2001-2019	Croatia

## 158	1970-2019	Taiwan
## 344	1986-2017	Hong Kong
## 376	1983-2016	Israel
## 410	2003-2018	Republic of Korea

## Test 2: Croatia
myLocations <- 191
## Test 1: Germany
myLocations <- 276


# Loop through each location with `lapply`
# This is useful if you want to work with many locations because
# the API can only handle a limited volume of queries at once.

## query on purpose some pre-1950 series in cases we have data gaps
## this can still be useful for interpolation to 1950

## get mx from complete / single age life table
tic()
myDT <- lapply( myLocations, function(x) {
	for (myLocID in x) {

		skip_to_next <- FALSE
		tryCatch(
			{query_func = get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),
                        startYear = 1920,
                        endYear = 2020,
                        indicatorIds = c(246), ## 246=mx (from life table), 248=qx, 256=lx for abridged age groups
                        # isComplete = 0,       ## 0=Abridged or 1=Complete
                        locIds = x,
                        locAreaTypeIds = 2,     ## "Whole area"
                        subGroupIds = 2,        ## "Total or All groups"
						dataSourceShortNames = myDS,
						includeUncertainty = FALSE,
						collapse_id_name = FALSE)
			return(query_func)},

			error = function(e) { skip_to_next <<- TRUE})

		## ---- skip to next country if no data
		if(skip_to_next) { next }
	}
})
toc()
# Merge all separate country data frames into
# one data frame.
mx1_DB <- data.table(do.call(rbind, myDT))
if (nrow(mx1_DB)>0){
	mx1_DB <- deduplicates(mx1_DB)
}
## fwrite(mx1_DB, paste0("DD_2021_mx1_DB.csv"), append = FALSE, col.names = TRUE, bom = TRUE)


## get abridged LT indicators
tic()
myDT <- lapply( myLocations, function(x) {
	for (myLocID in x) {

		skip_to_next <- FALSE
		tryCatch(
			{query_func = get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),
                        startYear = 1920,
                        endYear = 2020,
                        indicatorIds = (245),  ## 219=Mx, 245=mx (from life table), 247=qx, 255=lx for abridged age groups
                        # isComplete = 0,       ## 0=Abridged or 1=Complete
                        locIds = x,
                        locAreaTypeIds = 2,     ## "Whole area"
                        subGroupIds = 2,        ## "Total or All groups"
						dataSourceShortNames = myDS,
						includeUncertainty = FALSE,
						collapse_id_name = FALSE)
			return(query_func)},

			error = function(e) { skip_to_next <<- TRUE})

		## ---- skip to next country if no data
		if(skip_to_next) { next }
	}
})
toc()
mx5_DB <- data.table(do.call(rbind, myDT))
if (nrow(mx5_DB)>0){
	mx5_DB <- deduplicates(mx5_DB)
}
## fwrite(mx5_DB, paste0("DD_2021_mx5_DB.csv"), append = TRUE, col.names = TRUE, bom = TRUE)

## for 1 location
years <- 1930:2020
## check years available for single age data
available_years1 <- sort(unique(mx1_DB[, trunc(TimeMid)]))
years[(!years %in% available_years1)]

## check years available for abridged age data
available_years5 <- sort(unique(mx5_DB[, trunc(TimeMid)]))
years[(!years %in% available_years5)]

## years available in abridged form only
available_years5[(!available_years5 %in% available_years1)]
## -> use Limited LC for earlier years to interpolate/extrapolate

## get e0
tic()
myDT <- lapply( myLocations, function(x) {
	for (myLocID in x) {

		skip_to_next <- FALSE
		tryCatch(
			{query_func = get_recorddata(dataProcessTypeIds = c(6, 7, 9, 10),
                        startYear = 1920,
                        endYear = 2020,
                        indicatorIds = c(272, 266),  ## Life expectancy at birth and E(x) - abridged
                        # isComplete = 0,       ## 0=Abridged or 1=Complete
                        locIds = x,
                        locAreaTypeIds = 2,     ## "Whole area"
                        subGroupIds = 2,        ## "Total or All groups"
						dataSourceShortNames = myDS,
						includeUncertainty = FALSE,
						collapse_id_name = FALSE)
			return(query_func)},

			error = function(e) { skip_to_next <<- TRUE})

		## ---- skip to next country if no data
		if(skip_to_next) { next }
	}
})
toc()
e0_DB <- data.table(do.call(rbind, myDT))
if (nrow(e0_DB)>0){
	e0_DB <- e0_DB[AgeStart==0]
	e0_DB <- deduplicates(e0_DB)
}

# Plot comparison between Estimates and Register series by sex
e0_DB[SexID<=2] %>%
  ggplot( aes(x=TimeMid, y=DataValue, group=DataCatalogShortName, color=DataCatalogShortName)) +
    geom_line() +
    facet_wrap(~SexName, scale="free")
