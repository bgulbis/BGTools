# check_pregnant.R

#' Check for pregnant patients
#'
#' \code{check_pregnant} takes data frames with diagnosis codes and labs and
#' returns a data frame with identifiers of pregnant patients
#'
#' This function takes data frames with ICD9/10 diagnosis codes and a data frame
#' with urine/serum pregnancy test results, and returns a data frame with a list
#' of all patients who are pregnant. This can be used to exclude patients from
#' research studies.
#'
#' @param labs A data frame with pregnancy test lab values
#' @param icd9 An optional data frame with ICD-9 diagnosis codes; will not check
#'   for the presence of ICD-9 codes indicating pregnancy if NULL (default)
#' @param icd10 An optional data frame with ICD-10 diagnosis codes; will not
#'   check for the presence of ICD-10 codes indicating pregnancy if NULL
#'   (default)
#'
#' @return A data frame
#'
#' @export
check_pregnant <- function(labs, icd9 = NULL, icd10 = NULL) {
    # these ccs codes correspond to pregnancy
    preg.codes <- tibble::data_frame(disease.state = "preg", type = "CCS",
                             code = as.character(c(177:188, 190:196)))

    # find any patients with matching icd9 codes
    if(!is.null(icd9)) {
        icd9 <- tidy_data(icd9, "icd9", ref.data = preg.codes)
    }

    # find any patients with matching icd10 codes
    if(!is.null(icd10)) {
        icd10 <- tidy_data(icd10, "icd10", ref.data = preg.codes)
    }

    # find any patients with a positive pregnancy test
    labs <- dplyr::filter_(labs, .dots = list(~lab.result == "Positive"))

    preg <- tibble::data_frame(pie.id = c(icd9$pie.id, icd10$pie.id, labs$pie.id))
}
