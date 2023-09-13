# Load libraries ---------------------------------------------------------------
print('Load libraries')

library(magrittr)

# Specify redaction threshold --------------------------------------------------
print('Specify redaction threshold')

threshold <- 6

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Specify arguments ------------------------------------------------------------
print('Specify arguments')

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  cohort <- "vax"
} else {
  cohort <- args[[1]]
}


# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_rds(paste0("output/input_",cohort,"_stage1.rds"))


# Create exposure indicator ----------------------------------------------------
print("Create exposure indicator")

df$exposed <- !is.na(df$exp_date_covid19_confirmed)

#Create CKD cohort flag --------------------------------------------------------

df$sub_bin_ckd <- !is.na(df$sub_bin_ckd)

# Define age groups ------------------------------------------------------------
print("Define age groups")

df$cov_cat_age_group <- ""
df$cov_cat_age_group <- ifelse(df$cov_num_age>=18 & df$cov_num_age<=29, "18-29", df$cov_cat_age_group)
df$cov_cat_age_group <- ifelse(df$cov_num_age>=30 & df$cov_num_age<=39, "30-39", df$cov_cat_age_group)
df$cov_cat_age_group <- ifelse(df$cov_num_age>=40 & df$cov_num_age<=49, "40-49", df$cov_cat_age_group)
df$cov_cat_age_group <- ifelse(df$cov_num_age>=50 & df$cov_num_age<=59, "50-59", df$cov_cat_age_group)
df$cov_cat_age_group <- ifelse(df$cov_num_age>=60 & df$cov_num_age<=69, "60-69", df$cov_cat_age_group)
df$cov_cat_age_group <- ifelse(df$cov_num_age>=70 & df$cov_num_age<=79, "70-79", df$cov_cat_age_group)
df$cov_cat_age_group <- ifelse(df$cov_num_age>=80 & df$cov_num_age<=89, "80-89", df$cov_cat_age_group)
df$cov_cat_age_group <- ifelse(df$cov_num_age>=90, "90+", df$cov_cat_age_group)

# Filter data and create cohorts -----------------------------------------------
print("Filter data")

df_ckd <- df[df$sub_bin_ckd,c("patient_id",
                  "exposed",
                  "cov_cat_sex",
                  "cov_cat_age_group",
                  "cov_cat_ethnicity",
                  "cov_cat_deprivation",
                  "cov_cat_smoking_status",
                  "cov_cat_region",
                  "cov_bin_carehome_status")] 

df_gen <- df[!df$sub_bin_ckd,c("patient_id",
                              "exposed",
                              "cov_cat_sex",
                              "cov_cat_age_group",
                              "cov_cat_ethnicity",
                              "cov_cat_deprivation",
                              "cov_cat_smoking_status",
                              "cov_cat_region",
                              "cov_bin_carehome_status")] 


df_ckd$All <- "All"
df_gen$All <- "All"



# Aggregate data ---------------------------------------------------------------
print("Aggregate data")

df_ckd <- tidyr::pivot_longer(df_ckd,
                              cols = setdiff(colnames(df_ckd),c("patient_id","exposed")),
                              names_to = "characteristic",
                              values_to = "subcharacteristic")

df_ckd$total <- 1

df_ckd <- aggregate(cbind(total, exposed) ~ characteristic + subcharacteristic, 
                    data = df_ckd,
                    sum)
print("CKD cohort complete")

df_gen <- tidyr::pivot_longer(df_gen,
                              cols = setdiff(colnames(df_gen),c("patient_id","exposed")),
                              names_to = "characteristic",
                              values_to = "subcharacteristic")

df_gen$total <- 1

df_gen <- aggregate(cbind(total, exposed) ~ characteristic + subcharacteristic, 
                    data = df_gen,
                    sum)

print("General population cohort complete")


# Tidy care home characteristic ------------------------------------------------
print("Remove extraneous information")

df_ckd <- df_ckd[!(df_ckd$characteristic=="cov_bin_carehome_status" & 
                     df_ckd$subcharacteristic=="FALSE"),]

df_ckd$subcharacteristic <- ifelse(df_ckd$characteristic=="cov_bin_carehome_status",
                                   "Care home resident",
                                   df_ckd$subcharacteristic)
print("CKD cohort complete")


df_gen <- df_gen[!(df_gen$characteristic=="cov_bin_carehome_status" & 
                     df_gen$subcharacteristic=="FALSE"),]

df_gen$subcharacteristic <- ifelse(df_gen$characteristic=="cov_bin_carehome_status",
                                   "Care home resident",
                                   df_gen$subcharacteristic)

print("General population cohort complete")

# Sort characteristics ---------------------------------------------------------
print("Sort characteristics")

df_ckd$characteristic <- factor(df_ckd$characteristic,
                                levels = c("All",
                                           "cov_cat_sex",
                                           "cov_cat_age_group",
                                           "cov_cat_ethnicity",
                                           "cov_cat_deprivation",
                                           "cov_cat_smoking_status",
                                           "cov_cat_region",
                                           "cov_bin_carehome_status"),
                                labels = c("All",
                                           "Sex",
                                           "Age, years",
                                           "Ethnicity",
                                           "Index of multuple deprivation quintile",
                                           "Smoking status",
                                           "Region",
                                           "Care home resident"))
print("CKD cohort complete")

df_gen$characteristic <- factor(df_gen$characteristic,
                                levels = c("All",
                                           "cov_cat_sex",
                                           "cov_cat_age_group",
                                           "cov_cat_ethnicity",
                                           "cov_cat_deprivation",
                                           "cov_cat_smoking_status",
                                           "cov_cat_region",
                                           "cov_bin_carehome_status"),
                                labels = c("All",
                                           "Sex",
                                           "Age, years",
                                           "Ethnicity",
                                           "Index of multuple deprivation quintile",
                                           "Smoking status",
                                           "Region",
                                           "Care home resident"))

print("General population cohort complete")


# Sort subcharacteristics ------------------------------------------------------
print("Sort subcharacteristics")

df_ckd$subcharacteristic <- factor(df_ckd$subcharacteristic,
                                   levels = c("All",
                                              "Female",
                                              "Male",
                                              "18-29",
                                              "30-39",
                                              "40-49",
                                              "50-59",
                                              "60-69",
                                              "70-79",
                                              "80-89",
                                              "90+",
                                              "White",
                                              "Mixed",
                                              "South Asian",
                                              "Black",
                                              "Other",
                                              "Missing",
                                              "1-2 (most deprived)",
                                              "3-4",
                                              "5-6",
                                              "7-8",
                                              "9-10 (least deprived)",
                                              "Never smoker",
                                              "Ever smoker",
                                              "Current smoker",
                                              "East",
                                              "East Midlands",
                                              "London",
                                              "North East",
                                              "North West",
                                              "South East",
                                              "South West",
                                              "West Midlands",
                                              "Yorkshire and The Humber",
                                              "Care home resident",
                                              "Missing"),
                                   labels = c("All",
                                              "Female",
                                              "Male",
                                              "18-29",
                                              "30-39",
                                              "40-49",
                                              "50-59",
                                              "60-69",
                                              "70-79",
                                              "80-89",
                                              "90+",
                                              "White",
                                              "Mixed",
                                              "South Asian",
                                              "Black",
                                              "Other",
                                              "Missing",
                                              "1: most deprived",
                                              "2",
                                              "3",
                                              "4",
                                              "5: least deprived",
                                              "Never smoker",
                                              "Former smoker",
                                              "Current smoker",
                                              "East",
                                              "East Midlands",
                                              "London",
                                              "North East",
                                              "North West",
                                              "South East",
                                              "South West",
                                              "West Midlands",
                                              "Yorkshire/Humber",
                                              "Care home resident",
                                              "Missing"))
print("CKD cohort complete")

df_gen$subcharacteristic <- factor(df_gen$subcharacteristic,
                                   levels = c("All",
                                              "Female",
                                              "Male",
                                              "18-29",
                                              "30-39",
                                              "40-49",
                                              "50-59",
                                              "60-69",
                                              "70-79",
                                              "80-89",
                                              "90+",
                                              "White",
                                              "Mixed",
                                              "South Asian",
                                              "Black",
                                              "Other",
                                              "Missing",
                                              "1-2 (most deprived)",
                                              "3-4",
                                              "5-6",
                                              "7-8",
                                              "9-10 (least deprived)",
                                              "Never smoker",
                                              "Ever smoker",
                                              "Current smoker",
                                              "East",
                                              "East Midlands",
                                              "London",
                                              "North East",
                                              "North West",
                                              "South East",
                                              "South West",
                                              "West Midlands",
                                              "Yorkshire and The Humber",
                                              "Care home resident",
                                              "Missing"),
                                   labels = c("All",
                                              "Female",
                                              "Male",
                                              "18-29",
                                              "30-39",
                                              "40-49",
                                              "50-59",
                                              "60-69",
                                              "70-79",
                                              "80-89",
                                              "90+",
                                              "White",
                                              "Mixed",
                                              "South Asian",
                                              "Black",
                                              "Other",
                                              "Missing",
                                              "1: most deprived",
                                              "2",
                                              "3",
                                              "4",
                                              "5: least deprived",
                                              "Never smoker",
                                              "Former smoker",
                                              "Current smoker",
                                              "East",
                                              "East Midlands",
                                              "London",
                                              "North East",
                                              "North West",
                                              "South East",
                                              "South West",
                                              "West Midlands",
                                              "Yorkshire/Humber",
                                              "Care home resident",
                                              "Missing"))

print("General population cohort complete")

# Sort data --------------------------------------------------------------------
print("Sort data")

df_ckd <- df_ckd[order(df_ckd$characteristic, df_ckd$subcharacteristic),]

print("CKD cohort complete")

df_gen <- df_gen[order(df_gen$characteristic, df_gen$subcharacteristic),]

print("General population cohort complete")

# Save Table 1 -----------------------------------------------------------------
print('Save Table 1')

write.csv(df_ckd, paste0("output/table1_",cohort,"_ckd.csv"), row.names = FALSE)

print("CKD cohort saved")

write.csv(df_gen, paste0("output/table1_",cohort,"_gen.csv"), row.names = FALSE)

print("General population cohort saved")

# Perform redaction ------------------------------------------------------------
print('Perform redaction')

df_ckd[,setdiff(colnames(df_ckd),c("characteristic","subcharacteristic"))] <- lapply(df_ckd[,setdiff(colnames(df_ckd),c("characteristic","subcharacteristic"))],
                                                                                     FUN=function(y){roundmid_any(as.numeric(y), to=threshold)})

print("CKD cohort redacted")

df_gen[,setdiff(colnames(df_gen),c("characteristic","subcharacteristic"))] <- lapply(df_gen[,setdiff(colnames(df_gen),c("characteristic","subcharacteristic"))],
                                                                                     FUN=function(y){roundmid_any(as.numeric(y), to=threshold)})

print("General cohort redacted")

# Calculate column percentages -------------------------------------------------

#CKD cohort

df_ckd$Npercent <- paste0(df_ckd$total,ifelse(df_ckd$characteristic=="All","",
                                              paste0(" (",round(100*(df_ckd$total / df_ckd[df_ckd$characteristic=="All","total"]),1),"%)")))

df_ckd <- df_ckd[,c("characteristic","subcharacteristic","Npercent","exposed")]
colnames(df_ckd) <- c("Characteristic","Subcharacteristic","N (%)","COVID-19 diagnoses")

#General population cohort

df_gen$Npercent <- paste0(df_gen$total,ifelse(df_gen$characteristic=="All","",
                                              paste0(" (",round(100*(df_gen$total / df_gen[df_gen$characteristic=="All","total"]),1),"%)")))

df_gen <- df_gen[,c("characteristic","subcharacteristic","Npercent","exposed")]
colnames(df_gen) <- c("Characteristic","Subcharacteristic","N (%)","COVID-19 diagnoses")

# Save Table 1 -----------------------------------------------------------------
print('Save rounded Table 1')

write.csv(df_ckd, paste0("output/table1_",cohort,"_ckd_rounded.csv"), row.names = FALSE)

write.csv(df_gen, paste0("output/table1_",cohort,"_gen_rounded.csv"), row.names = FALSE)
