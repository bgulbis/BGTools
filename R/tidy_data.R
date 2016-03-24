# tidy_data.R

#' Tidy data
#'
#' \code{tidy_data} tidy data from standard EDW queries
#'
#' This function calls the underlying tidy function based on the value passed to
#' the type parameter and returns the tidy data frame. Valid options for type
#' are: diagnosis, meds_cont, meds_outpt, meds_sched.
#'
#' @param type A character indicating what type of data is being tidied
#' @param ... parameters to pass on to the underlying tidy function
#'
#' @return A data frame
#'
#' @export
tidy_data <- function(type, ...) {
    # get list of parameters from ellipsis
    x <- list(...)

    # call the desired tidy function based on type
    if (type == "diagnosis") {
        y <- tidy_diagnosis(x$ref.data, x$pt.data, x$patients)
    } else if (type == "meds_outpt") {
        # need to pass options for class and home if included
        if ("home" %in% names(x)) {
            y <- tidy_meds_outpt(x$ref.data, x$pt.data, x$patients, x$home)
        } else {
            y <- tidy_meds_outpt(x$ref.data, x$pt.data, x$patients)
        }
    } else if (type == "meds_cont") {
        y <- tidy_meds_cont(x$ref.data, x$cont.data, x$sched.data, x$patients)
    } else if (type == "meds_sched") {
        y <- tidy_meds_sched(x$ref.data, x$sched.data, x$patients)
    } else {
        y <- "Invalid type"
    }

    return(y)
}

# Change NA to FALSE
fill_false <- function(y) {
    ifelse(is.na(y), FALSE, y)
}

#' Tidy diagnosis codes
#'
#' \code{tidy_diagnosis} determines which patients have the desired diagnosis
#'
#' This function takes a data frame with reference diagnosis codes and a data
#' frame with all patient diagnosis codes, and returns a data frame with a
#' logical for each disease state for each patient.
#'
#' @param ref.data A data frame with the desired diagnosis codes
#' @param pt.data A data frame with all patient diagnosis codes
#' @param patients A data frame with a column pie.id including all patients in
#'   study
#'
#' @return A data frame
#'
tidy_diagnosis <- function(ref.data, pt.data, patients) {
    # convert any CCS codes to ICD9
    lookup.codes <- icd9_lookup(ref.data)
    lookup.codes <- dplyr::ungroup(lookup.codes)
    dots <- list(~factor(disease.state))
    lookup.codes <- dplyr::mutate_(lookup.codes, .dots = setNames(dots, "disease.state"))

    # only use finalized diagnosis codes
    dots <- list(~diag.type != "Admitting", ~diag.type != "Working")
    x <- dplyr::filter_(pt.data, .dots = dots)

    # join with the lookup codes
    x <- dplyr::inner_join(x, lookup.codes, by = c("diag.code" = "icd9.code"))

    # add a column called value and assign as TRUE, to be used with spread
    dots <- lazyeval::interp("y", y = TRUE)
    x <- dplyr::mutate_(x, .dots = setNames(dots, "value"))

    # drop all columns except pie.id, disease state, and value
    x <- dplyr::select_(x, .dots = list("pie.id", "disease.state", "value"))

    # remove all duplicate pie.id / disease state combinations
    x <- dplyr::distinct_(x, .dots = list("pie.id", "disease.state"))

    # convert the data to wide format
    x <- tidyr::spread_(x, "disease.state", "value", fill = FALSE, drop = FALSE)

    # join with list of all patients, fill in values of FALSE for any patients
    # not in the data set
    pts <- dplyr::select_(patients, "pie.id")
    x <- dplyr::full_join(x, pts, by = "pie.id")
    x <- dplyr::mutate_each_(x, funs(fill_false), list(quote(-pie.id)))

    return(x)
}

#' Tidy outpatient medications
#'
#' \code{tidy_meds_outpt} determines which patients have the desired outpatient
#' medications
#'
#' This function takes a data frame with reference outpatient medications or
#' medication classes and a data frame with all patient outpatient medications,
#' and returns a data frame with a logical for each medication for each patient.
#' The data frame passed to ref.data should contain two columns: name and type.
#' The name column should contain either generic medication names or medication
#' classes. The type column should specify whether the value in name is a
#' "class" or "med".
#'
#' @param ref.data A data frame with two columns, name and type
#' @param pt.data A data frame with all outpatient medications
#' @param patients A data frame with a column pie.id including all patients in
#'   study
#' @param home optional logical indicating to look for home medications if TRUE
#'   or discharge medications if FALSE
#'
#' @return A data frame
#'
tidy_meds_outpt <- function(ref.data, pt.data, patients, home = TRUE) {
    # for any med classes, lookup the meds included in the class
    meds <- dplyr::filter_(ref.data, .dots = list(~type == "class"))
    meds <- med_lookup(meds$name)

    # join the list of meds with any indivdual meds included
    lookup.meds <- dplyr::filter_(ref.data, .dots = list(~type == "med"))
    lookup.meds <- c(lookup.meds$name, meds$med.name)

    # filter to either home medications or discharge medications
    if (home == TRUE) {
        dots <- list(~med.type == "Recorded / Home Meds")
    } else {
        dots <- list(~med.type == "Prescription / Discharge Order")
    }
    x <- dplyr::filter_(pt.data, .dots = dots)

    # make all meds lowercase for comparisons
    dots <- list(~stringr::str_to_lower(med))
    x <- dplyr::mutate_(x, .dots = setNames(dots, "med"))

    # filter to meds in lookup
    dots <- list(~med %in% lookup.meds)
    x <- dplyr::filter_(x, .dots = dots)

    # join with list of meds to get class names
    x <- dplyr::left_join(x, meds, by = c("med" = "med.name"))

    # use the medication name or class to group by
    dots <- list(~ifelse(is.na(med.class), med, med.class),
                 lazyeval::interp("y", y = TRUE))
    nm <- c("group", "value")
    x <- dplyr::mutate_(x, .dots = setNames(dots, nm))

    # select only the pie.id, group, and value columns
    x <- dplyr::select_(x, .dots = list("pie.id", "group", "value"))

    # remove any duplicate patient / group combinations
    x <- dplyr::distinct_(x, .dots = list("pie.id", "group"))

    # convert the data to wide format
    x <- tidyr::spread_(x, "group", "value", fill = FALSE, drop = FALSE)

    # join with list of all patients, fill in values of FALSE for any patients
    # not in the data set
    x <- dplyr::semi_join(x, patients, by = "pie.id")
    x <- dplyr::mutate_each_(x, funs(fill_false), list(quote(-pie.id)))

    x
}

#' Tidy continuous medications
#'
#' \code{tidy_meds_cont} determines which patients have the desired continuous
#' medications
#'
#' This function takes a data frame with reference medications or medication
#' classes and data frames with all continuous and scheduled medications, and
#' returns a data frame with only the desired medications for each patient. The
#' data frame passed to ref.data should contain three columns: name, type, and
#' group. The name column should contain either generic medication names or
#' medication classes. The type column should specify whether the value in name
#' is a "class" or "med". The group column should specify whether the medication
#' is a continous or scheduled medication.
#'
#' @param ref.data A data frame with three columns: name, type, and group
#' @param cont.data A data frame with all outpatient medications
#' @param sched.data A data frame with all outpatient medications
#' @param patients A data frame with a column pie.id including all patients in
#'   study
#'
#' @return A data frame
#'
tidy_meds_cont <- function(ref.data, cont.data, sched.data, patients) {
    # filter to tidy only continuous meds
    ref.data <- dplyr::filter_(ref.data, .dots = list(~group == "cont"))

    # for any med classes, lookup the meds included in the class
    class.meds <- dplyr::filter_(ref.data, .dots = list(~type == "class"))
    class.meds <- med_lookup(class.meds$name)

    # join the list of meds with any indivdual meds included
    lookup.meds <- dplyr::filter_(ref.data, .dots = list(~type == "med"))
    lookup.meds <- c(lookup.meds$name, class.meds$med.name)

    # remove any rows in continuous data which are actually scheduled doses
    x <- dplyr::anti_join(cont.data, sched.data, by = "event.id")

    # make all meds lowercase for comparisons
    dots <- list(~stringr::str_to_lower(med))
    x <- dplyr::mutate_(x, .dots = setNames(dots, "med"))

    # filter to meds in lookup
    dots <- list(~med %in% lookup.meds)
    x <- dplyr::filter_(x, .dots = dots)

    # sort by pie.id, med, med.datetime
    x <- dplyr::arrange_(x, .dots = list("pie.id", "med", "med.datetime"))

    return(x)
}

#' Tidy scheduled medications
#'
#' \code{tidy_meds_sched} determines which patients have the desired medications
#'
#' This function takes a data frame with reference medications or medication
#' classes and a data frames with all scheduled medications, and
#' returns a data frame with only the desired medications for each patient. The
#' data frame passed to ref.data should contain three columns: name, type, and
#' group. The name column should contain either generic medication names or
#' medication classes. The type column should specify whether the value in name
#' is a "class" or "med". The group column should specify whether the medication
#' is a continous or scheduled medication.
#'
#' @param ref.data A data frame with three columns: name, type, and group
#' @param sched.data A data frame with all scheduled medications
#' @param patients A data frame with a column pie.id including all patients in
#'   study
#'
#' @return A data frame
#'
tidy_meds_sched <- function(ref.data, sched.data, patients) {
    # filter to tidy only scheduled meds
    ref.data <- dplyr::filter_(ref.data, .dots = list(~group == "sched"))

    # for any med classes, lookup the meds included in the class
    class.meds <- dplyr::filter_(ref.data, .dots = list(~type == "class"))
    class.meds <- med_lookup(class.meds$name)

    # join the list of meds with any indivdual meds included
    lookup.meds <- dplyr::filter_(ref.data, .dots = list(~type == "med"))
    lookup.meds <- c(lookup.meds$name, class.meds$med.name)

    # make all meds lowercase for comparisons
    dots <- list(~stringr::str_to_lower(med))
    x <- dplyr::mutate_(sched.data, .dots = setNames(dots, "med"))

    # filter to meds in lookup
    dots <- list(~med %in% lookup.meds)
    x <- dplyr::filter_(x, .dots = dots)

    # sort by pie.id, med, med.datetime
    x <- dplyr::arrange_(x, .dots = list("pie.id", "med", "med.datetime"))

    return(x)
}

