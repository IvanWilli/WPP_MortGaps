## This file loads all of the packages, data and functions needed to 
## implement SVDcomp model life tables for HIV countries

# root_dir <- "C:/Users/SARAH/OneDrive - United Nations/WPP2021/"
# input_file_path <- paste0(root_dir, "InputFiles/", "BWA_R21.xlsx")
# MORT_MODELS <- readxl::read_xlsx(path = input_file_path, sheet = "MORT_MODELS")
# 
# inputs <- MORT_MODELS

# Load the model data and RMS package
library(rms)
library(tidyverse)

if(!exists("mods.R")){load(file=paste0(root_dir, "MLT/SVDcomp_model_object/modsr-vr-dhs-spectrum-25-04.RData"))}

lt_model_hiv_SVDcomp <- function(inputs, # a data frame with time_start, time_span, sex, q5_value, q1_value, q1545_value, hivprev and artadult)
                                 adjust_oldage_lc = FALSE, # whether to use pre-hiv lee-carter extrapolations for older ages
                                 adjust_oldage_lc_fit_years = 1950:1985, # years to fit lee-carter extrapolations
                                 adjust_oldage_blend_ages = 65:80) { # ages to blend lee-carter extrapolations with SVDcomp mortality pattern

  
  # males
  
  # predict 1qx
  svd.m <- predictNQX("male", 
                      cm = inputs$q5_value[inputs$sex=="male"], 
                      am = inputs$q1545_value[inputs$sex=="male"], 
                      hiv = inputs$hivprev[inputs$sex=="male"], 
                      art = inputs$artadult[inputs$sex=="male"], adult = "q45", 
                      im = inputs$q1_value[inputs$sex=="male"])
  # inverse logit transforamtion
  svd.m <- expit(svd.m)
  # transpose so age in columns, country/period in rows
  svd.m <- as.data.frame(t(svd.m)) %>% 
    bind_cols(inputs[inputs$sex == "male",1:2]) %>% 
    select(c(111:112,1:110)) %>% 
    rename_at(3:112, ~paste0("age",0:109)) %>% # rename columns to ages
    mutate(age110 = 1) %>% # qx for open age group = 1
    gather(key = "Age", value = "qx", 3:113) %>% # make long with one row per Location, Period, Age
    mutate(Age = as.numeric(substr(Age, 4, nchar(Age))),
           sex = "male") 
  
  # females
  
  # predict 1qx
  svd.f <- predictNQX("female", 
                      cm = inputs$q5_value[inputs$sex=="female"], 
                      am = inputs$q1545_value[inputs$sex=="female"], 
                      hiv = inputs$hivprev[inputs$sex=="female"], 
                      art = inputs$artadult[inputs$sex=="female"], adult = "q45", 
                      im = inputs$q1_value[inputs$sex=="female"])
  # inverse logit transforamtion
  svd.f <- expit(svd.f)
  # transpose so age in columns, country/period in rows
  svd.f <- as.data.frame(t(svd.f)) %>% 
    bind_cols(inputs[inputs$sex == "female",1:2]) %>% 
    select(c(111:112,1:110)) %>% 
    rename_at(3:112, ~paste0("age",0:109)) %>% # rename columns to ages
    mutate(age110 = 1) %>% # qx for open age group = 1
    gather(key = "Age", value = "qx", 3:113) %>% # make long with one row per Location, Period, Age
    mutate(Age = as.numeric(substr(Age, 4, nchar(Age))),
           sex = "female") 
  
  # combine results for males and females
  svd_out <- rbind(svd.m, svd.f) %>% 
    arrange(time_start, sex, Age) %>% 
    mutate(id = paste(time_start, sex, sep = "-"))
  
  # compute life table values from output qx
  ids <- unique(svd_out$id)
  
  svd_lt <- list()
  
  for (i in 1:length(ids)) {
    
    df <- svd_out %>% 
      filter(id == ids[i])
    
    sex = substr(df$sex[1],1,1)
    
    lt <- DemoTools::lt_single_qx(nqx = df$qx, Age = df$Age, Sex = sex, a0rule = "ak", OAG = TRUE, OAnew = 110, parS = c(A = 0.005, B = 0.13))
    
    out <- df %>% 
      bind_cols(lt[,2:11])
    
    svd_lt[[i]] <- out
    
  }
  
  # compile results into a single data frame
  svd_lt <- do.call(rbind, svd_lt) %>% 
    arrange(sex, time_start, Age)
  
  if (adjust_oldage_lc) {
    
    # fit lee-carter extrapolation to some subset of the svd life tables
    svd_subset <- svd_lt %>% 
      filter(time_start %in% adjust_oldage_lc_fit_years) %>% 
      mutate(Date = as.Date(paste0(time_start,"-07-01")),
             Sex = substr(sex,1,1)) %>% 
      select(Date, Sex, Age, nMx, nqx, lx)
    
    dates_out <- unique(as.Date(paste0(svd_lt$time_start[!(svd_lt$time_start %in% adjust_oldage_lc_fit_years)],"-07-01")))
    
    # Avoiding cross-over between sex.
    lc <- DemoTools::interp_lc_lim(input = svd_subset, dates_out = dates_out, OAG = TRUE,
                                   prev_divergence = TRUE, Single = TRUE)$lt_hat 
    
    lc_merge <- lc %>% 
      mutate(time_start = floor(Date),
             sex = ifelse(Sex == "m", "male", "female")) %>% 
      select(time_start, sex, Age, nMx) %>% 
      rename(nMx_lc = nMx)
    
    svd_lt <- merge(svd_lt, lc_merge, by = c("time_start", "sex", "Age"), all.x = TRUE, all.y = TRUE)
    
  
  svd_lt <- svd_lt %>% 
    mutate(weight = 1,
           weight = replace(weight, Age %in% adjust_oldage_blend_ages, c(1, 1 - cumsum(rep(1 / (length(adjust_oldage_blend_ages)-1), length(adjust_oldage_blend_ages)-1)))),
           weight = replace(weight, Age > max(adjust_oldage_blend_ages), 0),
           nMx_blend = (nMx * weight) + (nMx_lc * (1-weight)),
           nMx_new = ifelse(is.na(nMx_blend), nMx, nMx_blend))
  
  ids <- unique(svd_lt$id)
  
  lts_lc <- list()
  for (i in 1:length(ids)) {
    
    df <- svd_lt %>% filter(id == ids[i]) %>% select(id, time_start, sex, Age, nMx_new, nqx)
    
    Sex <- substr(df$sex[1],1,1)
    
    lt <- DemoTools::lt_single_mx(nMx = df$nMx_new, Age= df$Age, a0rule = "ak", Sex = Sex, OAG = TRUE, IMR = df$nqx[1], parS = c(A = 0.005, B = 0.13)) %>% 
      mutate(time_start = df$time_start,
             time_span = df$time_span,
             sex = df$sex)
    
    lts_lc[[i]] <- lt
  }
  
  svd_lt <- do.call(rbind, lts_lc)
  } # end lee-carter blend for old-age adjustment
  
  svd_lt <- svd_lt[,c("time_start","sex","Age","AgeInt","nMx","nAx","nqx","lx","ndx","nLx","Sx","Tx","ex")]
  
  return(svd_lt)
  
}

# Helper functions

# function for inverse logit transformation 
expit <- function(x) { 
  return(exp(x)/(1 + exp(x))) 
}

# function for logit transformation 
logit <- function(x) {
  return(log(x/(1 - x))) 
}


############################################################################
# Function build
############################################################################

error.svd <- function(weights, sex, q5.ref, qadult.ref, adult, q0.ref) {
  
  adult_q <- "none"
  if (qadult.ref!=-9999 & adult == "q45") adult_q <- "q45"
  if (qadult.ref!=-9999 & adult == "q35") adult_q <- "q35"
  
  # Predict qx values
  r.p <- matrix(data = 0, ncol = 1, nrow = dim(mods.r[[sex]]$components)[2])
  for (z in 1:4) {
    r.p <- r.p + mods.r[[sex]]$components[z,] * weights[z]
    # r.p <- r.p + mods.r[[sex]]$components[z,] %*% t(weights[,z])
  }
  r.p <- r.p + mods.r[[sex]][[adult]]$offset
  
  r.p <- data.frame(r.p)
  
  # Splice in q0
  r.p[1,] <- q0.ref
  
  r.p.exp <- expit(r.p)
  
  # Predict q5
  q5.pred <- 1-sapply(1-r.p.exp[1:5,,drop=FALSE], prod)
  
  # Predict qAdult
  if (adult_q == "q45") {
    qadult.pred <- 1-sapply(1-r.p.exp[16:60,,drop=FALSE], prod)
  } else if (adult_q == "q35" ) {
    qadult.pred <- 1-sapply(1-r.p.exp[16:50,,drop=FALSE], prod)
  } else {
    qadult.pred <- qadult.ref <- 0
  }
  # Calculate and return the errors
  return((abs(q5.ref-q5.pred)*.1)+(abs(qadult.ref-qadult.pred)*.01))
}

## sex="female"; cm=hiv.countries.f$q5; hiv=hiv.countries.f$hiv; art=hiv.countries.f$art; adult="q35"; am=NULL
predictNQX <- function(sex, cm, am=NULL, hiv, art, adult, im=NULL) {
  # sex: "female" or "male"
  # cm: vector of 5q0 values
  # am: vector of 45q15 or 35q15 values
  # hiv: vector of HIV values (%)
  # art: vector of ART values (%)
  # adult: "q45" or "q35"
  # im: vector of 1q0 values
  
  # Logit transform
  cml <- logit(cm)
  
  # predict 1q0
  if(missing(im)) {
    cmls <- cml^2
    preds.q0 <- data.frame(
      cml = as.numeric(cml),
      cmls = as.numeric(cmls)
    )
    iml <- predict(mods.r[[sex]]$q0, newdata=preds.q0)
  } else{
    iml <- logit(im)
  }
  
  # predict adult mx
  if(missing(am)) {
    preds.aml <- data.frame(
      cm = as.numeric(cm),
      cml = as.numeric(cml),
      hiv = as.numeric(hiv),
      art = as.numeric(art)
    )
    aml <- predict(mods.r[[sex]][[adult]]$aml, newdata=preds.aml)	
  } else {
    aml <- logit(am)
  }
  
  preds.vs <- data.frame(
    cml = as.numeric(cml),
    aml = as.numeric(aml),
    hiv = as.numeric(hiv),
    art = as.numeric(art)
  )
  
  v1 <- predict(mods.r[[sex]][[adult]]$v1, newdata=preds.vs)
  v2 <- predict(mods.r[[sex]][[adult]]$v2, newdata=preds.vs)
  v3 <- predict(mods.r[[sex]][[adult]]$v3, newdata=preds.vs)
  v4 <- predict(mods.r[[sex]][[adult]]$v4, newdata=preds.vs)
  
  # adjust weights
  input.list <- list()
  for (i in 1:length(v1)) {   
    input.list[[i]] <- list(ws = c(v1[i], v2[i], v3[i], v4[i]),
                            q5.ref = cm[i], sex = sex, qadult.ref = am[i], 
                            adult = adult, q0.ref = iml[i])
    if(missing(am)) input.list[[i]]$qadult.ref <- -9999
    #            if(missing(am)) input.list[[i]]$qadult.ref <- expit(aml[i])
  } 
  
  opt.out <- lapply(input.list, function (x) { optim(x$ws, fn = error.svd, 
                                                     q5.ref = x$q5.ref, sex = x$sex, qadult.ref = x$qadult.ref, 
                                                     adult = x$adult, q0.ref = x$q0.ref,
                                                     lower=c((x$ws[1] - abs(0.0001*x$ws[1])), (x$ws[2] - abs(1*x$ws[2])), (x$ws[3] - abs(.25*x$ws[3])), (x$ws[4] - abs(.25*x$ws[4]))),
                                                     upper=c((x$ws[1] + abs(0.0001*x$ws[1])), (x$ws[2] + abs(1*x$ws[2])), (x$ws[3] + abs(.25*x$ws[3])), (x$ws[4] + abs(.25*x$ws[4]))),
                                                     method="L-BFGS-B")$par})
  names(opt.out) <- 1:length(opt.out)
  
  # Predict qx values
  v <- matrix(unlist(opt.out), ncol = length(cml))
  r.p <- t(mods.r[[sex]]$components) %*% v + mods.r[[sex]][[adult]]$offset
  r.p <- data.frame(r.p)
  # splice in q0, predicted or original
  r.p[1,] <- iml
  
  return(r.p)
  
}
