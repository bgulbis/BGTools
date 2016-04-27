# internal_data.R
#
# data to be used internally in package

# Clinical Classifications Software for ICD-10-CM/PCS
# Clinical Classificatiosn SOftware for ICD-9-CM
# by Healthcare Cost and Utilization Project
# available at https://www.hcup-us.ahrq.gov/

library(readr)
library(dplyr)
library(stringr)
library(devtools)

# Final ICD-9-CM codes as of 9/30/2015

ccs9.diagnosis <- read_csv("data-raw/$dxref 2015.csv.gz", skip = 1) %>%
    mutate_each(funs(str_replace_all(., "\'", "")), `'ICD-9-CM CODE'`, contains("CCS CATEGORY")) %>%
    mutate_each(funs(str_trim(., side = "both"))) %>%
    mutate(num = ifelse(str_detect(`'ICD-9-CM CODE'`, "^E"), 4, 3),
           len = str_length(`'ICD-9-CM CODE'`),
           icd.code = ifelse(str_length(`'ICD-9-CM CODE'`) > 3,
                             str_c(str_sub(`'ICD-9-CM CODE'`, end = num), ".",
                                   str_sub(`'ICD-9-CM CODE'`, start = num + 1)),
                             `'ICD-9-CM CODE'`)) %>%
    select(icd.code,
           icd.description = `'ICD-9-CM CODE DESCRIPTION'`,
           ccs.code = `'CCS CATEGORY'`,
           ccs.description = `'CCS CATEGORY DESCRIPTION'`)

ccs9.procedures <- read_csv("data-raw/$prref 2015.csv.gz", skip = 1) %>%
    mutate_each(funs(str_replace_all(., "\'", "")), `'ICD-9-CM CODE'`, contains("CCS CATEGORY")) %>%
    mutate_each(funs(str_trim(., side = "both"))) %>%
    mutate(icd.code = str_c(str_sub(`'ICD-9-CM CODE'`, end = 2), ".",
                             str_sub(`'ICD-9-CM CODE'`, start = 3))) %>%
    select(icd.code,
           icd.description = `'ICD-9-CM CODE DESCRIPTION'`,
           ccs.code = `'CCS CATEGORY'`,
           ccs.description = `'CCS CATEGORY DESCRIPTION'`)

# ICD-10-CM/PCS CCS codes valid through 10/1/2016

ccs10.diagnosis <- read_csv("data-raw/ccs_dx_icd10cm_2016.csv.gz") %>%
    mutate_each(funs(str_replace_all(., "\'", "")), `'ICD-10-CM CODE'`,
                `'CCS CATEGORY'`, contains("MULTI")) %>%
    mutate(icd.code = str_c(str_sub(`'ICD-10-CM CODE'`, end = 3), ".",
                              str_sub(`'ICD-10-CM CODE'`, start = 4))) %>%
    select(icd.code,
           icd.description = `'ICD-10-CM CODE DESCRIPTION'`,
           ccs.code = `'CCS CATEGORY'`,
           ccs.description = `'CCS CATEGORY DESCRIPTION'`)

#
ccs10.procedures <- read_csv("data-raw/ccs_pr_icd10pcs_2016.csv.gz") %>%
    mutate_each(funs(str_replace_all(., "\'", "")), `'ICD-10-PCS CODE'`,
                `'CCS CATEGORY'`, contains("MULTI")) %>%
    select(icd.code = `'ICD-10-PCS CODE'`,
           icd.description = `'ICD-10-PCS CODE DESCRIPTION'`,
           ccs.code = `'CCS CATEGORY'`,
           ccs.description = `'CCS CATEGORY DESCRIPTION'`)

# medication classes downloaded from EDW

med.classes <- read.csv("data-raw/medication_classes.csv", colClasses="character") %>%
    transmute(med.class = Drug.Catalog,
              med.name = Generic.Drug.Name)

devtools::use_data(ccs9.diagnosis, ccs9.procedures, ccs10.diagnosis,
                   ccs10.procedures, med.classes,
                   internal = TRUE, overwrite = TRUE)
