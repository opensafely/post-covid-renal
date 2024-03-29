# Based on common_variables in https://github.com/opensafely/post-covid-vaccinated/blob/main/analysis/common_variables.py

# Import statements

## Cohort extractor
from cohortextractor import (
    patients,
    codelist,
    filter_codes_by_category,
    combine_codelists,
    codelist_from_csv,
)

#study dates
from grouping_variables import (
    study_dates,
    days)
## Codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

## Datetime functions
from datetime import date

## Study definition helper
import study_definition_helper_functions as helpers

# Define pandemic_start
pandemic_start = study_dates["pandemic_start"]

# Define common variables function

def generate_common_variables(index_date_variable,exposure_end_date_variable,outcome_end_date_variable):
    dynamic_variables = dict(
# DEFINE EXPOSURES ------------------------------------------------------

    ## Date of positive SARS-COV-2 PCR antigen test
    tmp_exp_date_covid19_confirmed_sgss=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
        returning="date",
        find_first_match_in_period=True,
        date_format="YYYY-MM-DD",
        between=[f"{index_date_variable}",f"{exposure_end_date_variable}"],
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),
    ## First COVID-19 code (diagnosis, positive test or sequalae) in primary care
    tmp_exp_date_covid19_confirmed_snomed=patients.with_these_clinical_events(
        combine_codelists(
            covid_primary_care_code,
            covid_primary_care_positive_test,
            covid_primary_care_sequalae,
        ),
        returning="date",
        between=[f"{index_date_variable}",f"{exposure_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),
    ## Start date of episode with confirmed diagnosis in any position
    tmp_exp_date_covid19_confirmed_hes=patients.admitted_to_hospital(
        with_these_diagnoses=covid_codes,
        returning="date_admitted",
        between=[f"{index_date_variable}",f"{exposure_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),
    ## Date of death with SARS-COV-2 infection listed as primary or underlying cause
    tmp_exp_date_covid19_confirmed_death=patients.with_these_codes_on_death_certificate(
        covid_codes,
        returning="date_of_death",
        between=[f"{index_date_variable}",f"{exposure_end_date_variable}"],
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1
        },
    ),
    ## Generate variable to identify first date of confirmed COVID
    exp_date_covid19_confirmed=patients.minimum_of(
        "tmp_exp_date_covid19_confirmed_sgss","tmp_exp_date_covid19_confirmed_snomed","tmp_exp_date_covid19_confirmed_hes","tmp_exp_date_covid19_confirmed_death"
    ),

# POPULATION SELECTION VARIABLES ------------------------------------------------------

    has_follow_up_previous_6months=patients.registered_with_one_practice_between(
        start_date=f"{index_date_variable} - 6 months",
        end_date=f"{index_date_variable}",
        return_expectations={"incidence": 0.95},
    ),

    has_died = patients.died_from_any_cause(
        on_or_before = f"{index_date_variable}",
        returning="binary_flag",
        return_expectations={"incidence": 0.01}
    ),

    registered_at_start = patients.registered_as_of(f"{index_date_variable}",
    ),

    # Define subgroups (for variables that don't have a corresponding covariate only)
    ## COVID-19 severity
    sub_date_covid19_hospital = patients.admitted_to_hospital(
        with_these_primary_diagnoses=covid_codes,
        returning="date_admitted",
        on_or_after="exp_date_covid19_confirmed",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.5,
        },
    ),
    ## History of COVID-19 
    ### Positive SARS-COV-2 PCR antigen test
    tmp_sub_bin_covid19_confirmed_history_sgss=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
        returning='binary_flag',
        on_or_before=f"{index_date_variable} - 1 day",
        return_expectations={"incidence": 0.1},
    ),
    ### COVID-19 code (diagnosis, positive test or sequalae) in primary care
    tmp_sub_bin_covid19_confirmed_history_snomed=patients.with_these_clinical_events(
        combine_codelists(
            covid_primary_care_code,
            covid_primary_care_positive_test,
            covid_primary_care_sequalae,
        ),
        returning='binary_flag',
        on_or_before=f"{index_date_variable} - 1 day",
        return_expectations={"incidence": 0.1},
    ),
    ### Hospital episode with confirmed diagnosis in any position
    tmp_sub_bin_covid19_confirmed_history_hes=patients.admitted_to_hospital(
        with_these_diagnoses=covid_codes,
        returning='binary_flag',
        on_or_before=f"{index_date_variable} - 1 day",
        return_expectations={"incidence": 0.1},
    ),
    ## Generate variable to identify first date of confirmed COVID
    sub_bin_covid19_confirmed_history=patients.maximum_of(
        "tmp_sub_bin_covid19_confirmed_history_sgss","tmp_sub_bin_covid19_confirmed_history_snomed","tmp_sub_bin_covid19_confirmed_history_hes"
    ),



    #----------------------------------------------------------------------------------------
    # Renal Outcomes
    #----------------------------------------------------------------------------------------
    
    ## Creatinine measurements to be used for defining AKI and ESRD
    #out_num_creatinine=patients.mean_recorded_value(
    #        creatinine_codes,
    #        on_most_recent_day_of_measurement=True,
    #        on_or_before = f"{index_date_variable}- 1 day",
    #        return_expectations={
    #            "float": {"distribution": "normal", "mean": 80, "stddev": 40},
    #            "incidence": 0.60,
    #        }
    #    ),

    ## Acute kidney injury
    tmp_out_date_aki_snomed = patients.with_these_clinical_events(
        aki_snomed,
        returning="date",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_aki_hes = patients.admitted_to_hospital(
        with_these_diagnoses=aki_icd10,
        returning="date_admitted",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_aki_death=patients.with_these_codes_on_death_certificate(
        aki_icd10,
        returning="date_of_death",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ), 

    out_date_aki = patients.minimum_of(
        "tmp_out_date_aki_snomed","tmp_out_date_aki_hes","tmp_out_date_aki_death"
    ), 

    tmp_out_date_esrd_snomed = patients.with_these_clinical_events(
        esrd_snomed,
        returning="date",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_esrd_hes = patients.admitted_to_hospital(
        with_these_diagnoses=esrd_icd10,
        returning="date_admitted",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_esrd_death=patients.with_these_codes_on_death_certificate(
        esrd_icd10,
        returning="date_of_death",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ), 

    tmp_out_date_dialysis_snomed = patients.with_these_clinical_events(
        dialysis_snomed,
        returning="date",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_dialysis_hes = patients.admitted_to_hospital(
        with_these_diagnoses=dialysis_icd10,
        returning="date_admitted",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_dialysis_opcs = patients.admitted_to_hospital(
        with_these_procedures=dialysis_opcs,
        returning="date_admitted",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_dialysis_death=patients.with_these_codes_on_death_certificate(
        dialysis_icd10,
        returning="date_of_death",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ), 

    tmp_out_date_kidtrans_snomed = patients.with_these_clinical_events(
        kidtrans_snomed,
        returning="date",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_kidtrans_hes = patients.admitted_to_hospital(
        with_these_diagnoses=kidtrans_icd10,
        returning="date_admitted",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_kidtrans_opcs = patients.admitted_to_hospital(
        with_these_procedures=kidtrans_opcs,
        returning="date_admitted",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_kidtrans_death=patients.with_these_codes_on_death_certificate(
        kidtrans_icd10,
        returning="date_of_death",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ), 

    out_date_esrd = patients.minimum_of(
        "tmp_out_date_esrd_snomed","tmp_out_date_esrd_hes","tmp_out_date_esrd_death",
        "tmp_out_date_dialysis_snomed","tmp_out_date_dialysis_hes","tmp_out_date_dialysis_opcs","tmp_out_date_dialysis_death",
        "tmp_out_date_kidtrans_snomed","tmp_out_date_kidtrans_hes","tmp_out_date_kidtrans_opcs","tmp_out_date_kidtrans_death",
    ), 

    tmp_out_date_ckd34_snomed = patients.with_these_clinical_events(
        ckd34_snomed,
        returning="date",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_ckd34_hes = patients.admitted_to_hospital(
        with_these_diagnoses=ckd34_icd10,
        returning="date_admitted",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ),

    tmp_out_date_ckd34_death=patients.with_these_codes_on_death_certificate(
        ckd34_icd10,
        returning="date_of_death",
        between=[f"{index_date_variable}",f"{outcome_end_date_variable}"],
        match_only_underlying_cause=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
            "rate": "uniform",
            "incidence": 0.1,
        },
    ), 

    out_date_ckd = patients.minimum_of(
        "tmp_out_date_ckd34_snomed","tmp_out_date_ckd34_hes","tmp_out_date_ckd34_death","out_date_esrd",
    ), 

#---------------------
# Define populations
#---------------------

## Patients with a history of end stage renal disease - to be excluded
        #eGFR<15 to be added as a criteria for ESRD ??
    sub_bin_esrd = patients.categorised_as(
        {
            "1": "esrd_snomed OR esrd_icd10 OR dialysis_snomed OR dialysis_icd10 OR kidtrans_snomed OR kidtrans_icd10",  
            "0": "DEFAULT", 
        }, 
        esrd_snomed=patients.with_these_clinical_events(
            esrd_snomed,
            on_or_before = f"{index_date_variable} - 1 day",
            returning = "binary_flag",
            return_expectations={"incidence": 0.05}
        ),
        dialysis_snomed=patients.with_these_clinical_events(
            dialysis_snomed,
            on_or_before = f"{index_date_variable} - 1 day",
            returning = "binary_flag",
            return_expectations={"incidence": 0.05}
        ),
        kidtrans_snomed=patients.with_these_clinical_events(
            kidtrans_snomed,
            on_or_before = f"{index_date_variable} - 1 day",
            returning = "binary_flag",
            return_expectations={"incidence": 0.05}
        ),
        esrd_icd10=patients.admitted_to_hospital(
            with_these_diagnoses=esrd_icd10,
            on_or_before = f"{index_date_variable} - 1 day",
            date_format = "YYYY-MM-DD",
            returning = "binary_flag",
            return_expectations={"incidence": 0.05}
        ),
        dialysis_icd10=patients.admitted_to_hospital(
            with_these_diagnoses=dialysis_icd10,
            on_or_before = f"{index_date_variable} - 1 day",
            date_format = "YYYY-MM-DD",
            returning = "binary_flag",
            return_expectations={"incidence": 0.05}
        ),
        kidtrans_icd10=patients.admitted_to_hospital(
            with_these_diagnoses=kidtrans_icd10,
            on_or_before = f"{index_date_variable} - 1 day",
            date_format = "YYYY-MM-DD",
            returning = "binary_flag",
            return_expectations={"incidence": 0.05}
        ),
        return_expectations={
            "category":{"ratios": {"0": 0.9, "1": 0.1}}
        },
    ),

    ## Patients with a history of CKD
    sub_bin_ckd = patients.categorised_as(
        {
            "1": "ckd34_snomed OR ckd34_icd10 OR sub_bin_esrd=1",  #do we need to remove the last OR since we are removing those it doesn't matter so much
            "0": "DEFAULT", 
        }, 
        ckd34_snomed=patients.with_these_clinical_events(
            ckd34_snomed,
            on_or_before = f"{index_date_variable} - 1 day",
            returning = "binary_flag",
            return_expectations={"incidence": 0.05}
        ),
        ckd34_icd10=patients.admitted_to_hospital(
            with_these_diagnoses=ckd34_icd10,
            on_or_before = f"{index_date_variable} - 1 day",
            date_format = "YYYY-MM-DD",
            returning = "binary_flag",
            return_expectations={"incidence": 0.05}
        ),
        return_expectations={
            "category":{"ratios": {"0": 0.8, "1": 0.2}}
        },
    ),


# DEFINE COVARIATES ------------------------------------------------------

    ## Age
    cov_num_age = patients.age_as_of(
        f"{index_date_variable} - 1 day",
        return_expectations = {
        "rate": "universal",
        "int": {"distribution": "population_ages"},
        "incidence" : 0.001
        },
    ),

    ## Ethnicity 
    cov_cat_ethnicity=patients.categorised_as(
        helpers.generate_ethnicity_dictionary(6),
        cov_ethnicity_sus=patients.with_ethnicity_from_sus(
            returning="group_6", use_most_frequent_code=True
        ),
        cov_ethnicity_gp_opensafely=patients.with_these_clinical_events(
            opensafely_ethnicity_codes_6,
            on_or_before=f"{index_date_variable} - 1 day",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_primis=patients.with_these_clinical_events(
            primis_covid19_vacc_update_ethnicity,
            on_or_before=f"{index_date_variable} - 1 day",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_opensafely_date=patients.with_these_clinical_events(
            opensafely_ethnicity_codes_6,
            on_or_before=f"{index_date_variable} - 1 day",
            returning="category",
            find_last_match_in_period=True,
        ),
        cov_ethnicity_gp_primis_date=patients.with_these_clinical_events(
            primis_covid19_vacc_update_ethnicity,
            on_or_before=f"{index_date_variable} - 1 day",
            returning="category",
            find_last_match_in_period=True,
        ),
        return_expectations=helpers.generate_universal_expectations(5,True),
    ),

    ## Deprivation
    cov_cat_deprivation=patients.categorised_as(
        helpers.generate_deprivation_ntile_dictionary(10),
        index_of_multiple_deprivation=patients.address_as_of(
            f"{index_date_variable} - 1 day",
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
        ),
        return_expectations=helpers.generate_universal_expectations(10,False),
    ),

    ## Region
    cov_cat_region=patients.registered_practice_as_of(
        f"{index_date_variable} - 1 day",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.1,
                    "Yorkshire and The Humber": 0.1,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East": 0.1,
                    "London": 0.2,
                    "South East": 0.1,
                    "South West": 0.1,
                },
            },
        },
    ),

    ## Smoking status
    cov_cat_smoking_status=patients.categorised_as(
        {
            "S": "most_recent_smoking_code = 'S'",
            "E": """
                most_recent_smoking_code = 'E' OR (
                most_recent_smoking_code = 'N' AND ever_smoked
                )
            """,
            "N": "most_recent_smoking_code = 'N' AND NOT ever_smoked",
            "M": "DEFAULT",
        },
        return_expectations={
            "category": {"ratios": {"S": 0.6, "E": 0.1, "N": 0.2, "M": 0.1}}
        },
        most_recent_smoking_code=patients.with_these_clinical_events(
            smoking_clear,
            find_last_match_in_period=True,
            on_or_before=f"{index_date_variable} - 1 day",
            returning="category",
        ),
        ever_smoked=patients.with_these_clinical_events(
            filter_codes_by_category(smoking_clear, include=["S", "E"]),
            on_or_before=f"{index_date_variable} - 1 day",
        ),
    ),
 
    ## Care home status
    cov_bin_carehome_status=patients.care_home_status_as_of(
        f"{index_date_variable} -1 day", 
        categorised_as={
            "TRUE": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='Y'
              AND LocationRequiresNursing='N'
            """,
            "TRUE": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='N'
              AND LocationRequiresNursing='Y'
            """,
            "TRUE": "IsPotentialCareHome",
            "FALSE": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"TRUE": 0.30, "FALSE": 0.70},},
        },
    ),

    ## 2019 consultation rate
        cov_num_consulation_rate=patients.with_gp_consultations(
            between=[days(study_dates["pandemic_start"],-365), days(study_dates["pandemic_start"],-1)],
            returning="number_of_matches_in_period",
            return_expectations={
                "int": {"distribution": "poisson", "mean": 5},
            },
        ),

    ## Healthcare worker    
    cov_bin_healthcare_worker=patients.with_healthcare_worker_flag_on_covid_vaccine_record(
        returning='binary_flag', 
        return_expectations={"incidence": 0.01},
    ),
    
    ## Dementia

        ### Primary care
        tmp_cov_bin_dementia_snomed=patients.with_these_clinical_events(
            dementia_snomed_clinical,
            returning='binary_flag',
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),
        ### SUS
        tmp_cov_bin_dementia_hes=patients.admitted_to_hospital(
            returning='binary_flag',
            with_these_diagnoses=dementia_icd10,
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),
        ### Primary care - vascular
        tmp_cov_bin_dementia_vascular_snomed=patients.with_these_clinical_events(
            dementia_vascular_snomed_clinical,
            returning='binary_flag',
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),
        ### SUS - vascular
        tmp_cov_bin_dementia_vascular_hes=patients.admitted_to_hospital(
            returning='binary_flag',
            with_these_diagnoses=dementia_vascular_icd10,
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),
        ### Combined
        cov_bin_dementia=patients.maximum_of(
            "tmp_cov_bin_dementia_snomed", "tmp_cov_bin_dementia_hes", "tmp_cov_bin_dementia_vascular_snomed", "tmp_cov_bin_dementia_vascular_hes",
        ),    

    ## Liver disease

        ### Primary care
        tmp_cov_bin_liver_disease_snomed=patients.with_these_clinical_events(
            liver_disease_snomed_clinical,
            returning='binary_flag',
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),

        ### SUS
        tmp_cov_bin_liver_disease_hes=patients.admitted_to_hospital(
            returning='binary_flag',
            with_these_diagnoses=liver_disease_icd10,
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),
                
        ### Combined
        cov_bin_liver_disease=patients.maximum_of(
            "tmp_cov_bin_liver_disease_snomed", "tmp_cov_bin_liver_disease_hes",
        ),

    ## Cancer

        ### Primary care
        tmp_cov_bin_cancer_snomed=patients.with_these_clinical_events(
            cancer_snomed_clinical,
            returning='binary_flag',
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),

        ### SUS
        tmp_cov_bin_cancer_hes=patients.admitted_to_hospital(
            returning='binary_flag',
            with_these_diagnoses=cancer_icd10,
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),

        ### Combined
        cov_bin_cancer=patients.maximum_of(
            "tmp_cov_bin_cancer_snomed", "tmp_cov_bin_cancer_hes",
        ),

    ## Hypertension

        ### Primary care
        tmp_cov_bin_hypertension_snomed=patients.with_these_clinical_events(
            hypertension_snomed_clinical,
            returning='binary_flag',
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),

        ### SUS
        tmp_cov_bin_hypertension_hes=patients.admitted_to_hospital(
            returning='binary_flag',
            with_these_diagnoses=hypertension_icd10,
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),

        ### DMD
        tmp_cov_bin_hypertension_drugs_dmd=patients.with_these_medications(
            hypertension_drugs_dmd,
            returning='binary_flag',
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),
                
        ### Combined
        cov_bin_hypertension=patients.maximum_of(
            "tmp_cov_bin_hypertension_snomed", "tmp_cov_bin_hypertension_hes", "tmp_cov_bin_hypertension_drugs_dmd",
        ),

    ## Diabetes
            
        ### Type 1 diabetes primary care
        cov_bin_diabetes_type1_snomed=patients.with_these_clinical_events(
            diabetes_type1_snomed_clinical,
            returning='binary_flag',
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),

        ### Type 1 diabetes SUS
        cov_bin_diabetes_type1_hes=patients.admitted_to_hospital(
            returning='binary_flag',
            with_these_diagnoses=diabetes_type1_icd10,
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),

        ### Type 2 diabetes primary care
        cov_bin_diabetes_type2_snomed=patients.with_these_clinical_events(
            diabetes_type2_snomed_clinical,
            returning='binary_flag',
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),

        ### Type 2 diabetes SUS
        cov_bin_diabetes_type2_hes=patients.admitted_to_hospital(
            returning='binary_flag',
            with_these_diagnoses=diabetes_type2_icd10,
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),

        ### Other or non-specific diabetes
        cov_bin_diabetes_other=patients.with_these_clinical_events(
            diabetes_other_snomed_clinical,
            returning='binary_flag',
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),

        ### Gestational diabetes
        cov_bin_diabetes_gestational=patients.with_these_clinical_events(
            diabetes_gestational_snomed_clinical,
            returning='binary_flag',
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.1},
        ),

        ### Diabetes medication
        tmp_cov_bin_insulin_snomed=patients.with_these_medications(
            insulin_snomed_clinical,
            returning="binary_flag",
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.05},
        ),
        tmp_cov_bin_antidiabetic_drugs_snomed=patients.with_these_medications(
            antidiabetic_drugs_snomed_clinical,
            returning="binary_flag",
            on_or_before=f"{index_date_variable} - 1 day",
            return_expectations={"incidence": 0.05},
        ),

        ### Any diabetes covariate
        cov_bin_diabetes=patients.maximum_of(
            "cov_bin_diabetes_type1_snomed", "cov_bin_diabetes_type1_hes",
            "cov_bin_diabetes_type1_snomed", "cov_bin_diabetes_type1_hes", 
            "cov_bin_diabetes_type1_snomed", "cov_bin_diabetes_type1_hes", 
            "cov_bin_diabetes_type2_snomed", "cov_bin_diabetes_type2_hes",
            "cov_bin_diabetes_other", "cov_bin_diabetes_gestational",
            "tmp_cov_bin_insulin_snomed", "tmp_cov_bin_antidiabetic_drugs_snomed",
        ),

    ## Obesity
    ### Primary care
    tmp_cov_bin_obesity_snomed=patients.with_these_clinical_events(
        bmi_obesity_snomed_clinical,
        returning='binary_flag',
        on_or_before=f"{index_date_variable} - 1 day",
        return_expectations={"incidence": 0.1},
    ),
    ### HES APC
    tmp_cov_bin_obesity_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses=bmi_obesity_icd10,
        on_or_before=f"{index_date_variable} -1 day",
        return_expectations={"incidence": 0.1},
    ),
    ### Combined
    cov_bin_obesity=patients.maximum_of(
        "tmp_cov_bin_obesity_snomed", "tmp_cov_bin_obesity_hes",
    ),

    ### Categorising BMI
    cov_num_bmi = patients.most_recent_bmi(
        on_or_before=f"{index_date_variable} - 1 day",
        minimum_age_at_measurement=18,
        include_measurement_date=True,
        date_format="YYYY-MM",
        return_expectations={
            "date": {"earliest": "2010-02-01", "latest": "2022-02-01"}, ##How do we obtain these dates ?
            "float": {"distribution": "normal", "mean": 28, "stddev": 8},
            "incidence": 0.7,
        },
    ),

    cov_cat_bmi_groups = patients.categorised_as(
        {
            "Underweight": "cov_num_bmi < 18.5 AND cov_num_bmi > 12", 
            "Healthy_weight": "cov_num_bmi >= 18.5 AND cov_num_bmi < 25", 
            "Overweight": "cov_num_bmi >= 25 AND cov_num_bmi < 30",
            "Obese": "cov_num_bmi >=30 AND cov_num_bmi <70", 
            "Missing": "DEFAULT", 
        }, 
        return_expectations = {
            "rate": "universal", 
            "category": {
                "ratios": {
                    "Underweight": 0.05, 
                    "Healthy_weight": 0.25, 
                    "Overweight": 0.3,
                    "Obese": 0.3,
                    "Missing": 0.1, 
                }
            },
        },
        
    ),


            ## Chronic obstructive pulmonary disease
            
                ### Primary care
                tmp_cov_bin_chronic_obstructive_pulmonary_disease_snomed=patients.with_these_clinical_events(
                    copd_snomed_clinical,
                    returning='binary_flag',
                    on_or_before=f"{index_date_variable} - 1 day",
                    return_expectations={"incidence": 0.1},
                ),

                ### HES APC
                tmp_cov_bin_chronic_obstructive_pulmonary_disease_hes=patients.admitted_to_hospital(
                    returning='binary_flag',
                    with_these_diagnoses= copd_icd10,
                    on_or_before=f"{index_date_variable} - 1 day",
                    return_expectations={"incidence": 0.1},
                ),
                
                ### Combined
                cov_bin_chronic_obstructive_pulmonary_disease=patients.maximum_of(
                    "tmp_cov_bin_chronic_obstructive_pulmonary_disease_snomed", "tmp_cov_bin_chronic_obstructive_pulmonary_disease_hes",
                ),

            ## Acute myocardial infarction

                ### Primary care
                tmp_cov_bin_ami_snomed=patients.with_these_clinical_events(
                    ami_snomed_clinical,
                    returning='binary_flag',
                    on_or_before=f"{index_date_variable} - 1 day",
                    return_expectations={"incidence": 0.1},
                ),

                ### SUS
                tmp_cov_bin_ami_prior_hes=patients.admitted_to_hospital(
                    returning='binary_flag',
                    with_these_diagnoses=ami_prior_icd10,
                    on_or_before=f"{index_date_variable} - 1 day",
                    return_expectations={"incidence": 0.1},
                ),
                tmp_cov_bin_ami_hes=patients.admitted_to_hospital(
                    returning='binary_flag',
                    with_these_diagnoses=ami_icd10,
                    on_or_before=f"{index_date_variable} - 1 day",
                    return_expectations={"incidence": 0.1},
                ),

                ### Combined
                cov_bin_ami=patients.maximum_of(
                    "tmp_cov_bin_ami_snomed", "tmp_cov_bin_ami_prior_hes", "tmp_cov_bin_ami_hes",
                ),

            ## Ischeamic stroke

                ### Primary care
                tmp_cov_bin_stroke_isch_snomed=patients.with_these_clinical_events(
                    stroke_isch_snomed_clinical,
                    returning='binary_flag',
                    on_or_before=f"{index_date_variable} - 1 day",
                    return_expectations={"incidence": 0.1},
                ),

                ### SUS
                tmp_cov_bin_stroke_isch_hes=patients.admitted_to_hospital(
                    returning='binary_flag',
                    with_these_diagnoses=stroke_isch_icd10,
                    on_or_before=f"{index_date_variable} - 1 day",
                    return_expectations={"incidence": 0.1},
                ),

                ### Combined 
                cov_bin_stroke_isch=patients.maximum_of(
                    "tmp_cov_bin_stroke_isch_hes", "tmp_cov_bin_stroke_isch_snomed",
                ),

    ## History of AKI
    cov_bin_aki = patients.categorised_as(
        {
            "1": "aki_snomed OR aki_icd10",  
            "0": "DEFAULT", 
        }, 
        aki_snomed=patients.with_these_clinical_events(
            aki_snomed,
            on_or_before = f"{index_date_variable} - 1 day",
            returning = "binary_flag",
            return_expectations={"incidence": 0.05}
        ),
        aki_icd10=patients.admitted_to_hospital(
            with_these_diagnoses=aki_icd10,
            on_or_before = f"{index_date_variable} - 1 day",
            date_format = "YYYY-MM-DD",
            returning = "binary_flag",
            return_expectations={"incidence": 0.05}
        ),
        return_expectations={
            "category":{"ratios": {"0": 0.8, "1": 0.2}}
        },
    ),

### Define quality assurances
    ## Prostate cancer
        ### Primary care
        prostate_cancer_snomed=patients.with_these_clinical_events(
            prostate_cancer_snomed_clinical,
            returning='binary_flag',
            return_expectations={
                "incidence": 0.03,
            },
        ),
        ### HES APC
        prostate_cancer_hes=patients.admitted_to_hospital(
            with_these_diagnoses=prostate_cancer_icd10,
            returning='binary_flag',
            return_expectations={
                "incidence": 0.03,
            },
        ),
        ### ONS
        prostate_cancer_death=patients.with_these_codes_on_death_certificate(
            prostate_cancer_icd10,
            returning='binary_flag',
            return_expectations={
                "incidence": 0.02
            },
        ),
        ### Combined
        qa_bin_prostate_cancer=patients.maximum_of(
            "prostate_cancer_snomed", "prostate_cancer_hes", "prostate_cancer_death"
        ),

    ## Pregnancy
        qa_bin_pregnancy=patients.with_these_clinical_events(
            pregnancy_snomed_clinical,
            returning='binary_flag',
            return_expectations={
                "incidence": 0.03,
            },
        ),
    
    ## Combined oral contraceptive pill
    ### dmd: dictionary of medicines and devices
    cov_bin_combined_oral_contraceptive_pill=patients.with_these_medications(
        cocp_dmd, 
        returning='binary_flag',
        on_or_before=f"{index_date_variable} - 1 day",
        return_expectations={"incidence": 0.3},
    ),

    ## Hormone replacement therapy
    cov_bin_hormone_replacement_therapy=patients.with_these_medications(
        hrt_dmd, 
        returning='binary_flag',
        on_or_before=f"{index_date_variable} - 1 day",

        return_expectations={"incidence": 0.3},
    ),

    ## Year of birth
    qa_num_birth_year=patients.date_of_birth(
        date_format="YYYY",
        return_expectations={
            "date": {"earliest": study_dates["earliest_expec"], "latest": "today"},
            "rate": "uniform",
        },
    ),


        )

    return dynamic_variables