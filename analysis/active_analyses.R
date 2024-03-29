
    library(jsonlite)
    library(dplyr)

    # Create output directory ------------------------------------------------------
    fs::dir_create(here::here("lib"))

    # Create empty data frame ------------------------------------------------------

    df <- data.frame(cohort = character(),
                    exposure = character(), 
                    outcome = character(), 
                    ipw = logical(), 
                    strata = character(),
                    covariate_sex = character(),
                    covariate_age = character(),
                    covariate_other = character(),
                    cox_start = character(),
                    cox_stop = character(),
                    study_start = character(),
                    study_stop = character(),
                    cut_points = character(),
                    controls_per_case = numeric(),
                    total_event_threshold = numeric(),
                    episode_event_threshold = numeric(),
                    covariate_threshold = numeric(),
                    age_spline = logical(),
                    ckd_group = character(),
                    analysis = character(),
                    stringsAsFactors = FALSE)

    # Set constant values ----------------------------------------------------------

    ipw <- TRUE
    age_spline <- TRUE
    exposure <- "exp_date_covid19_confirmed"
    strata <- "cov_cat_region"
    covariate_sex <- "cov_cat_sex"
    covariate_age <- "cov_num_age"
    cox_start <- "index_date"
    cox_stop <- "end_date_outcome"
    controls_per_case <- 20L
    total_event_threshold <- 50L
    episode_event_threshold <- 5L
    covariate_threshold <- 5L

    ##Dates

    study_dates <- fromJSON("output/study_dates.json")    
    
    prevax_start <- "2020-01-01"
    prevax_stop<- "2021-12-14"
    vax_unvax_start<-"2021-06-01"
    vax_unvax_stop <-"2021-12-14"
    ##Cut points 
    prevax_cuts <- "28;197;365;714"
    vax_unvax_cuts <- "28;197"
    
    # Specify cohorts --------------------------------------------------------------

    cohorts <- c("vax","unvax","prevax")
    
    # Specify CKD populationg roups
    
    ckd_groups <-c("gen", "ckd_hist")

    # Specify outcomes -------------------------------------------------------------

    
    outcomes_runall <- c("out_date_ckd",
                         "out_date_aki",
                         "out_date_esrd")


    all_covars <- c("cov_cat_ethnicity;cov_cat_deprivation;cov_cat_smoking_status;cov_bin_carehome_status;cov_num_consulation_rate;cov_bin_healthcare_worker;cov_bin_dementia;cov_bin_liver_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_ami;cov_bin_stroke_isch;cov_bin_aki")
    
    
    # Add active analyses ----------------------------------------------------------

    for (c in cohorts) {
      
      for (ck in ckd_groups){
      
        for (i in outcomes_runall) {
        
        
        ## analysis: main ----------------------------------------------------------
        
        df[nrow(df)+1,] <- c(cohort = c,
                            exposure = exposure, 
                            outcome = i,
                            ipw = ipw, 
                            strata = strata,
                            covariate_sex = covariate_sex,
                            covariate_age = covariate_age,
                            covariate_other = "cov_cat_ethnicity;cov_cat_deprivation;cov_cat_smoking_status;cov_bin_carehome_status;cov_num_consulation_rate;cov_bin_healthcare_worker;cov_bin_dementia;cov_bin_liver_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_chronic_obstructive_pulmonary_disease;cov_bin_ami;cov_bin_stroke_isch;cov_bin_aki",
                            cox_start = cox_start,
                            cox_stop = cox_stop,
                            study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                            study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                            cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                            controls_per_case = controls_per_case,
                            total_event_threshold = total_event_threshold,
                            episode_event_threshold = episode_event_threshold,
                            covariate_threshold = covariate_threshold,
                            age_spline = TRUE,
                            ckd_group = ck,
                            analysis = "main")
        
        ## analysis: sub_covid_hospitalised ----------------------------------------
        
        df[nrow(df)+1,] <- c(cohort = c,
                            exposure = exposure, 
                            outcome = i,
                            ipw = ipw, 
                            strata = strata,
                            covariate_sex = covariate_sex,
                            covariate_age = covariate_age,
                            covariate_other = all_covars,
                            cox_start = cox_start,
                            cox_stop = cox_stop,
                            study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                            study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                            cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                            controls_per_case = controls_per_case,
                            total_event_threshold = total_event_threshold,
                            episode_event_threshold = episode_event_threshold,
                            covariate_threshold = covariate_threshold,
                            age_spline = TRUE,
                            ckd_group = ck,
                            analysis = "sub_covid_hospitalised")
        
        ## analysis: sub_covid_nonhospitalised -------------------------------------
        
        df[nrow(df)+1,] <- c(cohort = c,
                            exposure = exposure, 
                            outcome = i,
                            ipw = ipw, 
                            strata = strata,
                            covariate_sex = covariate_sex,
                            covariate_age = covariate_age,
                            covariate_other = all_covars,
                            cox_start = cox_start,
                            cox_stop = cox_stop,
                            study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                            study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                            cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                            controls_per_case = controls_per_case,
                            total_event_threshold = total_event_threshold,
                            episode_event_threshold = episode_event_threshold,
                            covariate_threshold = covariate_threshold,
                            age_spline = TRUE,
                            ckd_group = ck,
                            analysis = "sub_covid_nonhospitalised")
        
        ## analysis: sub_covid_history ---------------------------------------------
        
        if (c!="prevax") {
          
          df[nrow(df)+1,] <- c(cohort = c,
                              exposure = exposure, 
                              outcome = i,
                              ipw = ipw, 
                              strata = strata,
                              covariate_sex = covariate_sex,
                              covariate_age = covariate_age,
                              covariate_other = all_covars,
                              cox_start = cox_start,
                              cox_stop = cox_stop,
                              study_start =  vax_unvax_start,
                              study_stop =  vax_unvax_stop,
                              cut_points = vax_unvax_cuts,
                              controls_per_case = controls_per_case,
                              total_event_threshold = total_event_threshold,
                              episode_event_threshold = episode_event_threshold,
                              covariate_threshold = covariate_threshold,
                              age_spline = TRUE,
                              ckd_group = ck,
                              analysis = "sub_covid_history")
          
         }
        
      }
        for (i in outcomes_runall) {
          
          ## analysis: sub_sex_female ------------------------------------------------
          
          df[nrow(df)+1,] <- c(cohort = c,
                               exposure = exposure, 
                               outcome = i,
                               ipw = ipw, 
                               strata = strata,
                               covariate_sex = "NULL",
                               covariate_age = covariate_age,
                               covariate_other = all_covars,
                               cox_start = cox_start,
                               cox_stop = cox_stop,
                               study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                               study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                               cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                               controls_per_case = controls_per_case,
                               total_event_threshold = total_event_threshold,
                               episode_event_threshold = episode_event_threshold,
                               covariate_threshold = covariate_threshold,
                               age_spline = TRUE,
                               ckd_group = ck,
                               analysis = "sub_sex_female")
          
          ## analysis: sub_sex_male --------------------------------------------------
          
          df[nrow(df)+1,] <- c(cohort = c,
                               exposure = exposure, 
                               outcome = i,
                               ipw = ipw, 
                               strata = strata,
                               covariate_sex = "NULL",
                               covariate_age = covariate_age,
                               covariate_other = all_covars,
                               cox_start = cox_start,
                               cox_stop = cox_stop,
                               study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                               study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                               cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                               controls_per_case = controls_per_case,
                               total_event_threshold = total_event_threshold,
                               episode_event_threshold = episode_event_threshold,
                               covariate_threshold = covariate_threshold,
                               age_spline = TRUE,
                               ckd_group = ck,
                               analysis = "sub_sex_male")
          
          ## analysis: sub_age_18_39 ------------------------------------------------
          
          df[nrow(df)+1,] <- c(cohort = c,
                               exposure = exposure, 
                               outcome = i,
                               ipw = ipw, 
                               strata = strata,
                               covariate_sex = covariate_sex,
                               covariate_age = covariate_age,
                               covariate_other = all_covars,
                               cox_start = cox_start,
                               cox_stop = cox_stop,
                               study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                               study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                               cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                               controls_per_case = controls_per_case,
                               total_event_threshold = total_event_threshold,
                               episode_event_threshold = episode_event_threshold,
                               covariate_threshold = covariate_threshold,
                               age_spline = FALSE,
                               ckd_group = ck,
                               analysis = "sub_age_18_39")
          
          ## analysis: sub_age_40_59 ------------------------------------------------
          
          df[nrow(df)+1,] <- c(cohort = c,
                               exposure = exposure, 
                               outcome = i,
                               ipw = ipw, 
                               strata = strata,
                               covariate_sex = covariate_sex,
                               covariate_age = covariate_age,
                               covariate_other = all_covars,
                               cox_start = cox_start,
                               cox_stop = cox_stop,
                               study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                               study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                               cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                               controls_per_case = controls_per_case,
                               total_event_threshold = total_event_threshold,
                               episode_event_threshold = episode_event_threshold,
                               covariate_threshold = covariate_threshold,
                               age_spline = FALSE,
                               ckd_group = ck,
                               analysis = "sub_age_40_59")
          
          ## analysis: sub_age_60_79 ------------------------------------------------
          
          df[nrow(df)+1,] <- c(cohort = c,
                               exposure = exposure, 
                               outcome = i,
                               ipw = ipw, 
                               strata = strata,
                               covariate_sex = covariate_sex,
                               covariate_age = covariate_age,
                               covariate_other = all_covars,
                               cox_start = cox_start,
                               cox_stop = cox_stop,
                               study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                               study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                               cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                               controls_per_case = controls_per_case,
                               total_event_threshold = total_event_threshold,
                               episode_event_threshold = episode_event_threshold,
                               covariate_threshold = covariate_threshold,
                               age_spline = FALSE,
                               ckd_group = ck,
                               analysis = "sub_age_60_79")
          
          ## analysis: sub_age_80_110 ------------------------------------------------
          
          df[nrow(df)+1,] <- c(cohort = c,
                               exposure = exposure, 
                               outcome = i,
                               ipw = ipw, 
                               strata = strata,
                               covariate_sex = covariate_sex,
                               covariate_age = covariate_age,
                               covariate_other = all_covars,
                               cox_start = cox_start,
                               cox_stop = cox_stop,
                               study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                               study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                               cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                               controls_per_case = controls_per_case,
                               total_event_threshold = total_event_threshold,
                               episode_event_threshold = episode_event_threshold,
                               covariate_threshold = covariate_threshold,
                               age_spline = FALSE,
                               ckd_group = ck,
                               analysis = "sub_age_80_110")
          
          ## analysis: sub_ethnicity_white -------------------------------------------
          
          df[nrow(df)+1,] <- c(cohort = c,
                               exposure = exposure, 
                               outcome = i,
                               ipw = ipw, 
                               strata = strata,
                               covariate_sex = covariate_sex,
                               covariate_age = covariate_age,
                               covariate_other =gsub("cov_cat_ethnicity;","",all_covars), #-ethnicity,
                               cox_start = cox_start,
                               cox_stop = cox_stop,
                               study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                               study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                               cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                               controls_per_case = controls_per_case,
                               total_event_threshold = total_event_threshold,
                               episode_event_threshold = episode_event_threshold,
                               covariate_threshold = covariate_threshold,
                               age_spline = TRUE,
                               ckd_group = ck,
                               analysis = "sub_ethnicity_white")
          
          ## analysis: sub_ethnicity_black -------------------------------------------
          
          df[nrow(df)+1,] <- c(cohort = c,
                               exposure = exposure, 
                               outcome = i,
                               ipw = ipw, 
                               strata = strata,
                               covariate_sex = covariate_sex,
                               covariate_age = covariate_age,
                               covariate_other = gsub("cov_cat_ethnicity;","",all_covars),# -ethnicity,
                               cox_start = cox_start,
                               cox_stop = cox_stop,
                               study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                               study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                               cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                               controls_per_case = controls_per_case,
                               total_event_threshold = total_event_threshold,
                               episode_event_threshold = episode_event_threshold,
                               covariate_threshold = covariate_threshold,
                               age_spline = TRUE,
                               ckd_group = ck,
                               analysis = "sub_ethnicity_black")
          
          ## analysis: sub_ethnicity_mixed -------------------------------------------
          
          df[nrow(df)+1,] <- c(cohort = c,
                               exposure = exposure, 
                               outcome = i,
                               ipw = ipw, 
                               strata = strata,
                               covariate_sex = covariate_sex,
                               covariate_age = covariate_age,
                               covariate_other = gsub("cov_cat_ethnicity;","",all_covars),#-ethnicity
                               cox_start = cox_start,
                               cox_stop = cox_stop,
                               study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                               study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                               cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                               controls_per_case = controls_per_case,
                               total_event_threshold = total_event_threshold,
                               episode_event_threshold = episode_event_threshold,
                               covariate_threshold = covariate_threshold,
                               age_spline = TRUE,
                               ckd_group = ck,
                               analysis = "sub_ethnicity_mixed")
          
          ## analysis: sub_ethnicity_asian -------------------------------------------
          
          df[nrow(df)+1,] <- c(cohort = c,
                               exposure = exposure, 
                               outcome = i,
                               ipw = ipw, 
                               strata = strata,
                               covariate_sex = covariate_sex,
                               covariate_age = covariate_age,
                               covariate_other = gsub("cov_cat_ethnicity;","",all_covars),#-ethnicity
                               cox_start = cox_start,
                               cox_stop = cox_stop,
                               study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                               study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                               cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                               controls_per_case = controls_per_case,
                               total_event_threshold = total_event_threshold,
                               episode_event_threshold = episode_event_threshold,
                               covariate_threshold = covariate_threshold,
                               age_spline = TRUE,
                               ckd_group = ck,
                               analysis = "sub_ethnicity_asian")
          
          ## analysis: sub_ethnicity_other -------------------------------------------
          
          df[nrow(df)+1,] <- c(cohort = c,
                               exposure = exposure, 
                               outcome = i,
                               ipw = ipw, 
                               strata = strata,
                               covariate_sex = covariate_sex,
                               covariate_age = covariate_age,
                               covariate_other = gsub("cov_cat_ethnicity;","",all_covars),#-ethnicity
                               cox_start = cox_start,
                               cox_stop = cox_stop,
                               study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                               study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                               cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                               controls_per_case = controls_per_case,
                               total_event_threshold = total_event_threshold,
                               episode_event_threshold = episode_event_threshold,
                               covariate_threshold = covariate_threshold,
                               age_spline = TRUE,
                               ckd_group = ck,
                               analysis = "sub_ethnicity_other")
          
        } 
      }
     } 
    

    # Assign unique name -----------------------------------------------------------

    df$name <- paste0("cohort_",df$cohort, "-", 
                      df$analysis, "-", df$ckd_group, "-",
                      gsub("out_date_","",df$outcome))

    ## Remove the two models that did not work due to low counts prior to the outcome matching below
    
    df <- df %>%
          filter(name!="cohort_unvax-sub_covid_hospitalised-ckd_hist-esrd") %>%
          filter(name!= "cohort_prevax-sub_covid_hospitalised-ckd_hist-esrd")
    
    ## Remove models where the CKD population doesn't match the outcome
    
    #create two dataframes with the unwanted models 
    #(df2 - ckd history population and ckd outcome)
    #(df3 - general population and esrd outcome)
    
    df2= filter(df, outcome == "out_date_ckd" & ckd_group == "ckd_hist")
    df3= filter(df, outcome == "out_date_esrd" & ckd_group == "gen")
    
    #create a new dataframe using 'anti-join' to only keep the observations
    #that don't match in the original dataframe
    
    df4=dplyr::anti_join(df,df2) #keep those from df that don't match those in df2
    df5=dplyr::anti_join(df4,df3) #keep those from df4 that don't match those in df3
    
    
    #to save changing further code, consolidate df4 into df
    
    df = df5
    
    #remove unneeded dfs
    
    rm(df2,df3,df4,df5)
    
    # Check names are unique and save active analyses list -------------------------

    if (length(unique(df$name))==nrow(df)) {
      saveRDS(df, file = "lib/active_analyses.rds", compress = "gzip")
    } else {
      stop(paste0("ERROR: names must be unique in active analyses table"))
    }



