## compute Lee-Carter on 1x1 mx using demography package > forecast.lca

## using best fitting period and automatic breakpoints detection (from the strucchange package)



# List of packages for session

.packages = c("data.table", "DemoTools", "dplyr", "demography")



# Install CRAN packages (if not already installed)

.inst <- .packages %in% installed.packages()

if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],dependencies=TRUE)



# Load packages into session

lapply(.packages, require, character.only=TRUE)



## if necessary install manually DemoTools library (includes dependency on MortalityLaws for extension at odler ages

## library(devtools)

## Install_github("timriffe/DemoTools", force=TRUE)



## eventually use WPP 2019 1x1 pop as population exposure if necessary

DT1 <- fread("wpp2019_LT1-1950-2019.csv")

DT2 <- fread("wpp2019_LT1-2020-2099.csv")

DT <- rbind(DT1, DT2)

rm(DT1, DT2)



## fit Lee-Carter on period nMx

## demography > forecast.lca  Forecast demogdata data using Lee-Carter method.



## the Demography package requires/uses HMD data structure objects

## so your user-defined data must be first combined into such object structure/class:



get_demogdata <- function(Location, SexID, Age, Year, Mx, Exposure) {
     
     
     
     myDT <- data.table(SexID, Age, Year, Mx, Exposure)
     
     obj <- list(type="mortality", label=unique(myDT$Location), lambda=0)
     
     
     
     obj$year <- sort(unique(myDT$Year))
     
     obj$age <- sort(unique(myDT$Age))
     
     n <- length(obj$year)
     
     m <- length(obj$age)
     
     mnames <- c("male", "female", "total")
     
     obj$rate <- obj$pop <- list()
     
     
     
     obj$rate[[1]] <- as.matrix(dcast(myDT[SexID==1], Age ~ Year, value.var=c("Mx"))[1:m,2:(n+1)], nrow = m, ncol = n)
     
     obj$rate[[2]] <- as.matrix(dcast(myDT[SexID==2], Age ~ Year, value.var=c("Mx"))[1:m,2:(n+1)], nrow = m, ncol = n)
     
     obj$rate[[3]] <- as.matrix(dcast(myDT[SexID==3], Age ~ Year, value.var=c("Mx"))[1:m,2:(n+1)], nrow = m, ncol = n)
     
     
     
     obj$pop[[1]]  <- as.matrix(dcast(myDT[SexID==1], Age ~ Year, value.var=c("Exposure"))[1:m,2:(n+1)], nrow = m, ncol = n)
     
     obj$pop[[2]]  <- as.matrix(dcast(myDT[SexID==2], Age ~ Year, value.var=c("Exposure"))[1:m,2:(n+1)], nrow = m, ncol = n)
     
     obj$pop[[3]]  <- as.matrix(dcast(myDT[SexID==3], Age ~ Year, value.var=c("Exposure"))[1:m,2:(n+1)], nrow = m, ncol = n)
     
     
     
     dimnames(obj$rate[[1]]) <- dimnames(obj$pop[[1]]) <- list(obj$age, obj$year)
     
     dimnames(obj$rate[[2]]) <- dimnames(obj$pop[[2]]) <- list(obj$age, obj$year)
     
     dimnames(obj$rate[[3]]) <- dimnames(obj$pop[[3]]) <- list(obj$age, obj$year)
     
     names(obj$pop) = names(obj$rate) <- tolower(mnames)
     
     
     
     return(structure(obj, class = "demogdata"))
     
}





## fit Lee-Carter using best fitting period and automatic breakpoints detection (from the strucchange package)

## with a Minimum number of 20 years to include in fitting period



fit_lca <- function(Location, SexID, Age, Year, Mx, Exposure) {
     
     myDT <- get_demogdata(Location, SexID, Age, Year, Mx, Exposure)
     
     
     
     lca <- lca(myDT, max.age = 110, adjust = c("dt"), chooseperiod = TRUE, minperiod = 20, breakmethod = c("bai"), scale = FALSE, interpolate = FALSE, series="male")
     
     fcast <- forecast(lca, jumpchoice="actual", h = 50, series="male")
     
     MxM <- fcast$rate$male
     
     dimnames(MxM) <- list(fcast$age, fcast$year)
     
     
     
     lca <- lca(myDT, max.age = 110, adjust = c("dt"), chooseperiod = TRUE, minperiod = 20, breakmethod = c("bai"), scale = FALSE, interpolate = FALSE, series="female")
     
     fcast <- forecast(lca, jumpchoice="actual", h = 50, series="female")
     
     MxF <- fcast$rate$female
     
     dimnames(MxF) <- list(fcast$age, fcast$year)
     
     
     
     lca <- lca(myDT, max.age = 110, adjust = c("dt"), chooseperiod = TRUE, minperiod = 20, breakmethod = c("bai"), scale = FALSE, interpolate = FALSE, series="total")
     
     fcast <- forecast(lca, jumpchoice="actual", h = 50, series="total")
     
     MxB <- fcast$rate$total
     
     dimnames(MxB) <- list(fcast$age, fcast$year)
     
     
     
     output <- rbind(data.table(SexID=1, Age=rownames(MxM), MxM), data.table(SexID=2, Age=rownames(MxF), MxF), data.table(SexID=3, Age=rownames(MxB), MxB))
     
     output <- melt(data=output, id=c("SexID", "Age"), variable.name="Year", value.name="nMx", na.rm=TRUE)
     
     output[SexID==1, Sex := "Male"]
     
     output[SexID==2, Sex := "Female"]
     
     output[SexID==3, Sex := "Both"]
     
     output[, Age := as.numeric(Age)]
     
     output[, Year := as.numeric(as.character(Year))]
     
     
     
     return(output)
     
}



## fit Lee-Carter to compute forecasted Mx (by sex)

myDT <- DT[, fit_lca(Location, SexID, Age, Year, nMx, nLx), by=list(LocID, Location)]

## compute life tables

LT1 <- myDT[, lt_single_mx(nMx, Age, Sex = unique(tolower(substr(Sex, 1, 1))), OAnew = 110), by=list(LocID, Location, SexID, Sex, Year)]