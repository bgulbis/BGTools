# icd9_lookup.R

#' Lookup ICD9 codes from CCS codes
#'
#' \code{icd9_lookup} takes a data frame with CCS codes and returns the
#' corresponding ICD9 codes
#'
#' Deprecated function, use \code{\link{icd_lookup}} instead.
#'
#' This function takes a data frame with three columns: disease.state, type, and
#' code. The column \code{disease.state} is a character field with the name of a
#' disease state which will be used for grouping. The column \code{type} is a
#' character field with either "ICD9" or "CCS", which indicates the type of
#' code. The column \code{code} is a character field with the ICD9 or CCS code.
#' For all rows with CCS codes, the function will look-up the corresponding ICD9
#' code and then return a data frame with two columns: disease.state and
#' icd9.code. The procedure parameter is used to specify whether diagnosis codes
#' or procedure codes should be returned.
#'
#'
#' @param df A data frame with columns: disease.state, type, code
#' @param procedure A logical indicating whether to use diagnosis codes
#'   (default) or procedure codes
#'
#' @return A data frame with columns: disease.state, icd9.code
#'
#' @export
icd9_lookup <- function(df, procedure = FALSE) {
    warning("icd9_lookup is deprecated, use icd_lookup instead")

    if (procedure == TRUE) {
        data <- ccs9.procedures
    } else {
        data <- ccs9.diagnosis
    }

    # find the ICD9 codes for the desired exclusions by CCS code
    ccs <- dplyr::filter_(df, .dots = list(~type == "CCS"))
    # make all CCS codes numeric
    # dots <- list(~as.numeric(code))
    # ccs <- dplyr::mutate_(ccs, .dots = setNames(dots, "ccs.code"))
    ccs <- dplyr::rename_(ccs, .dots = setNames("code", "ccs.code"))
    # join CCS list with data
    ccs <- dplyr::inner_join(ccs, data, by = "ccs.code")

    # ICD9 codes for non-CCS code exclusions
    icd9 <- dplyr::filter_(df, .dots = list(~type == "ICD9"))
    # rename code column
    icd9 <- dplyr::rename_(icd9, .dots = setNames("code", "icd.code"))
    # join list with data
    icd <- dplyr::inner_join(icd, data, by = "icd.code")

    # create one table with all ICD9 codes that should be excluded
    codes <- dplyr::bind_rows(ccs, icd9)
    codes <- dplyr::rename_(codes, .dots = setNames("icd.code", "icd9.code"))
    # keep only the disease state and ICD9 code
    codes <- dplyr::select_(codes, .dots = list("disease.state", "icd9.code"))
    # group by disease state
    # codes <- dplyr::group_by_(codes, "disease.state")

    return(codes)
}


#' Lookup ICD9 code description
#'
#' \code{icd9_description} takes a vector of ICD9 codes and returns a data frame
#' with the corresponding description for each code
#'
#' Deprecated function, use \code{\link{icd_description}} instead.
#'
#' This function takes a character vector of ICD9 codes and returns a data frame
#' with two columns: icd9.code and icd9.description.
#'
#'
#' @param codes A character vector of ICD9 codes
#' @param procedure A logical indicating whether to use diagnosis codes
#'   (default) or procedure codes
#'
#' @return A data frame with columns: icd9.code and icd9.description
#'
#' @export
icd9_description <- function(codes, procedure = FALSE) {
    warning("icd9_description is deprecated, use icd_lookup instead")

    if (procedure == TRUE) {
        data <- ccs9.procedures
    } else {
        data <- ccs9.diagnosis
    }

    # get only the desired descriptions based on ICD9 codes
    descript <- dplyr::filter_(data, .dots = list(~icd.code %in% codes))
    # keep only ICD9 code and description columns
    dots <- list("icd.code", "icd.description")
    nm <- list("icd9.code", "icd9.description")
    descript <- dplyr::select_(descript, .dots = setNmaes(dots, nm))

    return(descript)
}
