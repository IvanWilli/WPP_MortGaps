

#root_dir <- "C:/Users/SARAH/OneDrive - United Nations/WPP2021/"
#excel_input_file_path <- paste0(root_dir,"InputFiles/CAN_R21.xlsx")
#excel_input_file_path <- paste0(root_dir,"MLT/test input excel files/BWA_R21.xlsx")
# mlt_workflow_one_location(excel_input_file_path, root_dir)


# model life table workflow for one location

mlt_workflow_one_location <- function(excel_input_file_path,
                                      root_dir) {
  
  source(paste0(root_dir, "LifeTables/MLT/R/WPP_MLT_mortality_patterns.R"))
  
  # read the mortality estimation parameters
  MORT_PARAMS <- readxl::read_xlsx(path = excel_input_file_path, sheet = "MORT_PARAMS")
  ## get information on the estimation
  mp <- MORT_PARAMS %>% 
    dplyr::filter(type == "Estimation") %>% 
    dplyr::select(parameter, value)
  
  model_region <- mp$value[mp$parameter == "Age_Specific_MLT_Region"]
  # for classic model life table families, we use the Coale-Demeny a0 rule, otherwise we use Andreev-Kinkaid
  a0rule <- ifelse(mp$value[mp$parameter == "Age_Specific_Mortality_Type"] == "Model-based" &
                     !is.na(mp$value[mp$parameter == "Age_Specific_MLT_Region"]) & 
                     model_region %in% 
                     c("CD_West","CD_East","CD_North","CD_South", "UN_Chilean","UN_Far_Eastern",
                       "UN_General","UN_Latin_American","UN_South_Asian"), "cd", "ak")
  
  if (mp$value[mp$parameter == "Age_Specific_Mortality_Type"] == "Model-based") {
    
    MORT_MODELS <- readxl::read_xlsx(path = excel_input_file_path, sheet = "MORT_MODELS")
    model_region <- mp$value[mp$parameter == "Age_Specific_MLT_Region"]
    
    # classic MLTs
    if (model_region %in% c("CD_West","CD_East","CD_North","CD_South", "UN_Chilean","UN_Far_Eastern",
                            "UN_General","UN_Latin_American","UN_South_Asian")) {
      
      mlts <- mortality_patterns_classicMLT(model_region = model_region,
                                            inputs_type = mp$value[mp$parameter == "Age_Specific_MLT_Type_of_Inputs"],
                                            model_inputs = MORT_MODELS[, c("time_start","sex","q1_value","q5_value","q1545_value","e0_value")])
    }
    
    # logquad
    if (model_region == "LogQuad") {
      
      mlts <- mortality_patterns_LogQuad(inputs_type = "5q0 and 45q15",
                                                  model_inputs = MORT_MODELS[, c("time_start","sex","q1_value","q5_value","q1545_value")])
      
    }
    
    # HIV SVDcomp
    if (model_region == "HIV_SVDcomp") {
      
      mlts <- mortality_patterns_SVDcomp(model_inputs = MORT_MODELS[, c("time_start","time_span","sex","q1_value","q5_value","q1545_value","hivprev","artadult")], 
                                                  adjust_oldage_lc = as.logical(mp$value[mp$parameter == "HIV_SVDcomp_AdjustOldAgeLC"]), # whether to use pre-hiv lee-carter extrapolations for older ages
                                                  adjust_oldage_lc_fit_years = eval(parse(text = mp$value[mp$parameter == "HIV_SVDcomp_AdjustOldAgeLC_FitYears"])), # years to fit lee-carter extrapolations
                                                  adjust_oldage_blend_ages = eval(parse(text = mp$value[mp$parameter == "HIV_SVDcomp_AdjustOldAgeLC_BlendAges"])))
      
    }
    
    # parse and reshape long female life tables
    mlts_f <- mlts$female
    mlts_f$AgeInt[is.na(mlts_f$AgeInt)] <- 1000
    mlts_f <- reshape(mlts_f, idvar = c("time_start", "Age", "AgeInt"), timevar = "indicator",
                      varying = list(names(mlts_f)[3:11]), times = paste0("lt_",names(mlts_f)[3:11]), 
                      direction = "long", v.names = "value")
    names(mlts_f)[1:2] <- c("age_start", "age_span")
    mlts_f$sex <- "female"
    mlts_f$time_span <- 1
    
    # parse and reshape long male life tables
    mlts_m <- mlts$male
    mlts_m$AgeInt[is.na(mlts_m$AgeInt)] <- 1000
    mlts_m <- reshape(mlts_m, idvar = c("time_start", "Age", "AgeInt"), timevar = "indicator",
                      varying = list(names(mlts_m)[3:11]), times = paste0("lt_",names(mlts_m)[3:11]), 
                      direction = "long", v.names = "value")
    names(mlts_m)[1:2] <- c("age_start", "age_span")
    mlts_m$sex <- "male"
    mlts_m$time_span <- 1
    
    life_table_age_sex <- rbind(mlts_f, mlts_m)[,c("indicator",	"time_start",	"time_span",	"sex",	"age_start",
                                                   "age_span",	"value")]
    
    # add crisis mortality, if indicated
    
    # identify year/sex combinations for which mortality crisis adjustment is needed
    MORT_INPUTS <- readxl::read_xlsx(path = excel_input_file_path, sheet = "MORT_INPUTS")
    mort_crises <- MORT_INPUTS[MORT_INPUTS$adjust_conflict_mortality == TRUE | 
                                 MORT_INPUTS$adjust_disasters_mortality == TRUE | 
                                 MORT_INPUTS$adjust_COVID19_mortality == TRUE,
                               c("time_start", "time_span", "sex")]
    mort_crises$id <- paste(mort_crises$time_start, mort_crises$time_span, mort_crises$sex, sep = "-")
    
    if (nrow(mort_crises) > 0) {
      
      # read the mx single year of age associated with mortality crises
      mx1_crises <- readxl::read_xlsx(path = excel_input_file_path, sheet = "mx1_crises")
      mx1_crises$id <- paste(mx1_crises$time_start, mx1_crises$time_span, mx1_crises$sex, sep = "-")
      mx1_crises <- mx1_crises[mx1_crises$id %in% mort_crises$id, c("id","time_start","sex","age_start","value")]
      mx1_crises$value[mx1_crises$value == 0] <- 0.000000000000001  # replace zero with very small value to avoid errors
      
      maxage_lt <- max(life_table_age_sex$age_start)
      maxage_crises <- max(mx1_crises$age_start)
      
      # get the region for life table cd rules
      region = "w"
      if (substr(model_region,1,2) == "CD") {
        region <- tolower(substr(model_region,4,4))
      }
      
      # ensure that the open age groups of the life table and crisis 1mx match
      if (maxage_crises != maxage_lt) {
        ids <- unique(mx1_crises$id)
        mx1_crises_OAnew <- list()
        for (i in 1:length(ids)) {
          
          mx_OAnew <- DemoTools::lt_single_mx(nMx = mx1_crises$value[mx1_crises$id == ids[i]],
                                              Age = mx1_crises$age_start[mx1_crises$id == ids[i]],
                                              Sex = substr(mx1_crises$sex[mx1_crises$id == ids[i]][1],1,1),
                                              OAnew = maxage_lt,
                                              a0rule = a0rule,
                                              region = region)[,c("Age","nMx")]
          names(mx_OAnew) <- c("age_start", "value")
          mx_OAnew$sex <- mx1_crises$sex[mx1_crises$id == ids[i]][1]
          mx_OAnew$time_start <- mx1_crises$time_start[mx1_crises$id == ids[i]][1]
          
          mx1_crises_OAnew[[i]] <- mx_OAnew 
          rm(mx_OAnew)
        }
        mx1_crises <- do.call(rbind, mx1_crises_OAnew)
        rm(ids)
      }
      mx1_crises$mx1_crises <- mx1_crises$value # rename the value field for merge with life tables
      mx1_crises <- mx1_crises[,c("time_start", "sex", "age_start", "mx1_crises")]
      
      lt_mx1_crises <- merge(life_table_age_sex[life_table_age_sex$indicator == "lt_nMx",], mx1_crises,
                             by = c("time_start", "sex", "age_start"), all.x = TRUE, all.y = FALSE)
      lt_mx1_crises$age_start <- as.numeric(lt_mx1_crises$age_start)
      lt_mx1_crises$mx1_crises[is.na(lt_mx1_crises$mx1_crises)] <- 0 # set nas to 0
      lt_mx1_crises$value <- lt_mx1_crises$value + lt_mx1_crises$mx1_crises # add crisis mx to nMx values
      
      lt_mx1_crises$id <- paste(lt_mx1_crises$time_start, lt_mx1_crises$sex, sep = "-")
      lt_mx1_crises <- lt_mx1_crises[order(lt_mx1_crises$time_start, lt_mx1_crises$sex, lt_mx1_crises$age_start),]
      
      # recompute the life tables with the nMx series that include crisis mortality
      ids <- unique( lt_mx1_crises$id)
      lt_crises <- list()
      for (i in 1:length(ids)) {
        
        lt_new <- DemoTools::lt_single_mx(nMx = lt_mx1_crises$value[lt_mx1_crises$id == ids[i]],
                                          Age = lt_mx1_crises$age_start[lt_mx1_crises$id == ids[i]],
                                          Sex = substr(lt_mx1_crises$sex[lt_mx1_crises$id == ids[i]][1],1,1),
                                          a0rule = a0rule,
                                          region = region)
        lt_new$sex <- lt_mx1_crises$sex[lt_mx1_crises$id == ids[i]][1]
        lt_new$time_start <- lt_mx1_crises$time_start[lt_mx1_crises$id == ids[i]][1]
        
        lt_crises[[i]] <- lt_new
        rm(lt_new)
      }
      lt_new <- do.call(rbind, lt_crises)
      rm(ids)
      
      # reshape long 
      lt_new$AgeInt[is.na(lt_new$AgeInt)] <- 1000
      lt_new <- reshape(lt_new, idvar = c("time_start", "sex", "Age", "AgeInt"), timevar = "indicator",
                        varying = list(names(lt_new)[3:11]), times = paste0("lt_",names(lt_new)[3:11]), 
                        direction = "long", v.names = "value")
      names(lt_new)[1:2] <- c("age_start", "age_span")
      lt_new$time_span <- 1
      
      life_table_age_sex <- lt_new[,c("indicator",	"time_start",	"time_span",	"sex",	"age_start",
                                      "age_span",	"value")]
      
      
    } # close if condition for mortality crises
    
    
    # write the model life tables back to the input excel file
    
    wb <- openxlsx::loadWorkbook(excel_input_file_path)
    openxlsx::deleteData(wb, sheet = "life_table_age_sex", cols = 1:7, rows = 2:1048576, gridExpand = TRUE) # delete all rows except the header
    openxlsx::writeData(wb, sheet = "life_table_age_sex", x =life_table_age_sex, startCol = 1, startRow = 2, colNames = FALSE)

    ## update_status worksheet
    update_status <- data.table::data.table(openxlsx::readWorkbook(xlsxFile = wb, sheet = "update_status"))
    # convert date columns
    changeCols <- colnames(update_status)[which(as.vector(update_status[,lapply(.SD, class)]) %in% c("logical", "numeric"))]
    update_status[,(changeCols):= lapply(.SD, as.character), .SDcols = changeCols]	
    
    now <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
    update_status[worksheet %in% c("life_table_age_sex"), last_update := now]
    openxlsx::writeData(wb, sheet = "update_status", update_status$last_update, startCol=3, startRow=2, colNames = FALSE, rowNames=FALSE)
    #openxlsx::setColWidths(wb, "update_status", 1:ncol(update_status), widths="auto") 
    
    openxlsx::saveWorkbook(wb, excel_input_file_path, overwrite = T)
    
    ## now let's also make some plots
    
    
  } else {
    print("Age_specific_mortality_type is 'Empirical'. No model life tables are estimated for this location.")
  }
  

  
}


# ## use openxlsx 4.2.3 (version openxlsx 4.2.4 has some bug with formatting issues that loose the default formatting of column headers)
# if (packageVersion('openxlsx') > '4.2.3'){
#   devtools::unload("openxlsx")
#   devtools::install_version("openxlsx", version='4.2.3', repos = "http://cran.us.r-project.org")
# }

## openxlsx converts columns of class "POSIxt" to Excel datetimes with the format given as
options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
options(scipen = 9999)

Sys.setenv(TZ = "America/New_York")
