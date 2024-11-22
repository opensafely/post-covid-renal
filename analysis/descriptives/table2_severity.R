# Load libraries ----

library(magrittr)
library(tidyverse)
library(purrr)
library(data.table)
library(svglite)
library(VennDiagram)
library(grid)
library(gridExtra)

# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_csv(path_model_output,
                      show_col_types = FALSE)

colnames(df) <- gsub("_midpoint6","",colnames(df))

#get/fix CKD groups

df$tmp <- str_split_fixed(df$name, "-",4)[,3]
df$population <- ""
df$population <- ifelse(df$tmp=="ckd_hist","gen",df$population)
df$population <- ifelse(df$tmp=="gen","ckd_hist",df$population)
#df[,c("tmp","name")] <- NULL

#remove results with no person time therefore unusable

df<- df[!is.na(df$person_time_total),]

#keep only max adj model data so we only have one of each

df <- df[df$model=="mdl_max_adj",]

#combine all of the days together to match expected formatting

df<- df %>%
    group_by(outcome, analysis, cohort, population) %>%
    summarise(N_event_tot = sum(N_events), N_person_time_tot = sum(person_time_total), N_totals = sum(N_total))


# Keep totals ------------------------------------------------------------------
print("Keep totals")

totals <- unique(df[df$analysis=="main",c("cohort","population", "N_total")])

totals <- tidyr::pivot_wider(totals,
                             names_from = "cohort",
                             values_from = c("N_total"))

colnames(totals) <- paste0("event_personyears_",colnames(totals))

totals$outcome_label <- "N"

# Filter data ------------------------------------------------------------------



##this filtering step doesn't seem to work, and it may be the issue?
#print("Filter data")

df <- df[df$analysis %in% c("main","sub_covid_hospitalised","sub_covid_nonhospitalised"),]

#df$events <- ifelse(df$analysis=="main", df$unexposed_events, df$exposed_events)
#df$person_days <- ifelse(df$analysis=="main", df$unexposed_person_days, df$exposed_person_days)

#df <- df[,c("cohort","analysis","outcome","events","person_days")]

# Add plot labels --------------------------------------------------------------
print("Add plot labels")

plot_labels <- readr::read_csv("lib/plot_labels.csv",
                               show_col_types = FALSE)

df$outcome <- gsub("out_date_","",df$outcome)
df <- merge(df, plot_labels[,c("term","label")], by.x = "outcome", by.y = "term", all.x = TRUE)
df <- dplyr::rename(df, "outcome_label" = "label")

df <- merge(df, plot_labels[,c("term","label")], by.x = "analysis", by.y = "term", all.x = TRUE)
df <- dplyr::rename(df, "covid19_severity" = "label")
df$covid19_severity <- ifelse(df$covid19_severity=="All COVID-19","No COVID-19",df$covid19_severity)
df$covid19_severity <- factor(df$covid19_severity, levels = c("No COVID-19","Hospitalised COVID-19","Non-hospitalised COVID-19"))

# Add other columns ------------------------------------------------------------
print("Add other columns")

df$event_personyears <- paste0(df$N_event_tot,"/", round((df$N_person_time_tot/365.25)))
df$incidencerate <- round(df$N_event_tot/((df$N_person_time_tot/365.25)/100000))

# Pivot table ------------------------------------------------------------------
print("Pivot table")

df <- df[,c("cohort","outcome_label","covid19_severity","event_personyears","incidencerate")]

df <- tidyr::pivot_wider(df, 
                         names_from = "cohort",
                         values_from = c("event_personyears","incidencerate"))

# Add totals to table ----------------------------------------------------------
print("Add totals to table")

df <- plyr::rbind.fill(totals, df)

# Order outcomes ---------------------------------------------------------------
print("Order outcomes")

df$outcome_label <- factor(df$outcome_label,
                           levels = c("N",
                                      "AKI",
                                      "CKD",
                                      "ESRD"
                                      ))

# Tidy table -------------------------------------------------------------------
print("Tidy table")

df <- df[order(df$outcome_label,df$covid19_severity),
         c("outcome_label","covid19_severity",
           paste0(c("event_personyears","incidencerate"),"_prevax_extf"),
           paste0(c("event_personyears","incidencerate"),"_vax"),
           paste0(c("event_personyears","incidencerate"),"_unvax_extf"))]

df <- dplyr::rename(df,
                    "Outcome" = "outcome_label",
                    "COVID-19 severity" = "covid19_severity")

# Save table -------------------------------------------------------------------
print("Save table")

readr::write_csv(df, "output/post_release/table2_severity.csv", na = "-")