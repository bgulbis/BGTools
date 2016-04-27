# icd9_lookup.R

#' Lookup ICD9/10 codes from CCS codes
#'
#' \code{icd_lookup} takes a data frame with CCS codes and returns the
#' corresponding ICD9 codes
#'
#' This function takes a data frame with three columns: disease.state, type, and
#' code. The column \code{disease.state} is a character field with the name of a
#' disease state which will be used for grouping. The column \code{type} is a
#' character field with either "ICD" or "CCS", which indicates the type of code.
#' The column \code{code} is a character field with the ICD or CCS code. For all
#' rows with CCS codes, the function will look-up the corresponding ICD9/10 code
#' and then return a data frame with two columns: disease.state and icd.code.
#' The procedure parameter is used to specify whether diagnosis codes or
#' procedure codes should be returned.
#'
#' @param df A data frame with columns: disease.state, type, code
#' @param icd10 A logical indicating whether to use ICD-10 codes (default) or
#'   ICD-9 codes
#' @param procedure A logical indicating whether to use diagnosis codes
#'   (default) or procedure codes
#'
#' @return A data frame
#'
#' @export
icd_lookup <- function(df, icd10 = FALSE, procedure = FALSE) {
    data <- get_icd_data(icd10, procedure)

    # find the ICD codes for the desired exclusions by CCS code
    ccs <- dplyr::filter_(df, .dots = list(~type == "CCS"))
    # make all CCS codes numeric
    # dots <- list(~as.numeric(code))
    # ccs <- dplyr::mutate_(ccs, .dots = setNames(dots, "ccs.code"))
    ccs <- dplyr::rename_(ccs, .dots = setNames("code", "ccs.code"))
    # join CCS list with data
    ccs <- dplyr::inner_join(ccs, data, by = "ccs.code")

    # ICD codes for non-CCS code exclusions
    icd <- dplyr::filter_(df, .dots = list(~type == "ICD"))
    # rename code column
    icd <- dplyr::rename_(icd, .dots = setNames("code", "icd.code"))
    # join list with data
    icd <- dplyr::inner_join(icd, data, by = "icd.code")

    # create one table with all ICD9 codes that should be excluded
    codes <- dplyr::bind_rows(ccs, icd)
    # keep only the disease state and ICD code
    codes <- dplyr::select_(codes, .dots = list("disease.state", "icd.code"))
}

#' Lookup ICD9/10 code description
#'
#' \code{icd_description} takes a vector of ICD codes and returns a data frame
#' with the corresponding description for each code
#'
#' This function takes a character vector of ICD codes and returns a data frame
#' with two columns: icd.code and icd.description.
#'
#'
#' @param codes A character vector of ICD9/10 codes
#' @param icd10 A logical indicating whether to use ICD-10 codes (default) or
#'   ICD-9 codes
#' @param procedure A logical indicating whether to use diagnosis codes
#'   (default) or procedure codes
#'
#' @return A data frame with columns: icd9.code and icd9.description
#'
#' @export
icd_description <- function(codes, icd10 = FALSE, procedure = FALSE) {
    data <- get_icd_data(icd10, procedure)

    # get only the desired descriptions based on ICD codes
    descript <- dplyr::filter_(data, .dots = list(~icd.code %in% codes))
    # keep only ICD code and description columns
    dots <- list("icd.code", "icd.description")
    descript <- dplyr::select_(descript, .dots = dots)
}

#' Lookup ICD9/10 codes by description
#'
#' \code{find_icd_codes} takes a regular expression and returns a data frame
#' with matching ICD9/10 codes
#'
#' This function takes a regular expression and matches it to the ICD9/10
#' description. A data frame containing the matching ICD9/10 codes is returned.
#'
#'
#' @param pattern A regular expression
#' @param icd10 A logical indicating whether to use ICD-10 codes (default) or
#'   ICD-9 codes
#' @param procedure A logical indicating whether to use diagnosis codes
#'   (default) or procedure codes
#'
#' @return A data frame
#'
#' @export
find_icd_codes <- function(pattern, icd10 = FALSE, procedure = FALSE) {
    data <- get_icd_data(icd10, procedure)

    expr <- stringr::regex(pattern, ignore_case = TRUE)
    dots <- list(~stringr::str_detect(icd.description, expr))
    # get only the desired descriptions based on ICD codes
    icd <- dplyr::filter_(data, .dots = dots)
}

#' Lookup ICD9/10 CCS codes by description
#'
#' \code{find_ccs_codes} takes a regular expression and returns a data frame
#' with matching CCS codes
#'
#' This function takes a regular expression and matches it to the CCS
#' description. A data frame containing the matching CCS codes is returned.
#'
#'
#' @param pattern A regular expression
#' @param icd10 A logical indicating whether to use ICD-10 codes (default) or
#'   ICD-9 codes
#' @param procedure A logical indicating whether to use diagnosis codes
#'   (default) or procedure codes
#'
#' @return A data frame
#'
#' @export
find_ccs_codes <- function(pattern, icd10 = FALSE, procedure = FALSE) {
    data <- get_icd_data(icd10, procedure)

    expr <- stringr::regex(pattern, ignore_case = TRUE)
    dots <- list(~stringr::str_detect(ccs.description, expr))
    # get only the desired descriptions based on ICD codes
    icd <- dplyr::filter_(data, .dots = dots)
}

# get the desired data set
get_icd_data <- function(icd10 = FALSE, procedure = FALSE) {
    if (icd10 == FALSE && procedure == TRUE) {
        data <- ccs9.procedures
    } else if (icd10 == TRUE && procedure == TRUE) {
        data <- ccs10.procedures
    } else if (icd10 == TRUE && procedure == FALSE) {
        data <- ccs10.diagnosis
    } else {
        data <- ccs9.diagnosis
    }

    data
}
