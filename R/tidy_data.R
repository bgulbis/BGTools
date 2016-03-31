# tidy_data.R

#' Tidy data
#'
#' \code{tidy_data} tidy data from standard EDW queries
#'
#' This function calls the underlying tidy function based on the value passed to
#' the type parameter and returns the tidy data frame. Valid options for type
#' are: diagnosis, locations, meds_cont, meds_outpt, meds_sched.
#'
#' @param raw.data A data frame with the data to be tidied
#' @param type A character indicating what type of data is being tidied
#' @param ... parameters to pass on to the underlying tidy function
#'
#' @return A data frame
#'
#' @export
tidy_data <- function(raw.data, type, ...) {
    home <- TRUE
    patients <- NULL

    # get list of parameters from ellipsis
    x <- list(...)
    list2env(x, environment())

    # call the desired tidy function based on type
    switch(type,
           diagnosis = tidy_diagnosis(raw.data, ref.data, patients),
           locations = tidy_locations(raw.data),
           meds_cont = tidy_meds_cont(raw.data, ref.data, sched.data),
           meds_outpt = tidy_meds_outpt(raw.data, ref.data, patients, home),
           meds_sched = tidy_meds_sched(raw.data, ref.data),
           stop("Invalid type")
    )
}

# Change NA to FALSE
fill_false <- function(y) {
    if (is.na(y)) FALSE else y
}

#' Tidy diagnosis codes
#'
#' \code{tidy_diagnosis} determines which patients have the desired diagnosis
#'
#' This function takes a data frame with reference diagnosis codes and a data
#' frame with all patient diagnosis codes, and returns a data frame with a
#' logical for each disease state for each patient.
#'
#' @param raw.data A data frame with all patient diagnosis codes
#' @param ref.data A data frame with the desired diagnosis codes
#' @param patients An optional data frame with a column pie.id including all
#'   patients in study
#'
#' @return A data frame
#'
tidy_diagnosis <- function(raw.data, ref.data, patients = NULL) {
    # convert any CCS codes to ICD9
    lookup.codes <- icd9_lookup(ref.data)
    lookup.codes <- dplyr::ungroup(lookup.codes)
    dots <- list(~factor(disease.state))
    nm <- "disease.state"
    lookup.codes <- dplyr::mutate_(lookup.codes, .dots = setNames(dots, nm))

    # only use finalized diagnosis codes
    dots <- list(~diag.type != "Admitting", ~diag.type != "Working")
    tidy <- dplyr::filter_(raw.data, .dots = dots)

    # join with the lookup codes
    tidy <- dplyr::inner_join(tidy, lookup.codes, by = c("diag.code" = "icd9.code"))

    # add a column called value and assign as TRUE, to be used with spread
    dots <- lazyeval::interp("y", y = TRUE)
    tidy <- dplyr::mutate_(tidy, .dots = setNames(dots, "value"))

    # drop all columns except pie.id, disease state, and value
    dots <- list("pie.id", "disease.state", "value")
    tidy <- dplyr::select_(tidy, .dots = dots)

    # remove all duplicate pie.id / disease state combinations
    dots <- list("pie.id", "disease.state")
    tidy <- dplyr::distinct_(tidy, .dots = dots)

    # convert the data to wide format
    tidy <- tidyr::spread_(tidy, "disease.state", "value", fill = FALSE, drop = FALSE)

    # join with list of all patients, fill in values of FALSE for any patients
    # not in the data set
    if (!is.null(patients)) {
        tidy <- dplyr::full_join(tidy, patients, by = "pie.id")
        tidy <- dplyr::mutate_each_(tidy, funs(fill_false), list(quote(-pie.id)))
    }

    tidy
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
#' @param raw.data A data frame with all outpatient medications
#' @param ref.data A data frame with two columns, name and type
#' @param patients An optinoal data frame with a column pie.id including all
#'   patients in study
#' @param home optional logical indicating to look for home medications if TRUE
#'   or discharge medications if FALSE
#'
#' @return A data frame
#'
tidy_meds_outpt <- function(raw.data, ref.data, patients = NULL, home = TRUE) {
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
    tidy <- dplyr::filter_(raw.data, .dots = dots)

    # filter to meds in lookup
    dots <- list(~med %in% lookup.meds)
    tidy <- dplyr::filter_(tidy, .dots = dots)

    # join with list of meds to get class names
    tidy <- dplyr::left_join(tidy, meds, by = c("med" = "med.name"))

    # use the medication name or class to group by
    dots <- list(~ifelse(is.na(med.class), med, med.class),
                 lazyeval::interp("y", y = TRUE))
    nm <- c("group", "value")
    tidy <- dplyr::mutate_(tidy, .dots = setNames(dots, nm))

    # select only the pie.id, group, and value columns
    tidy <- dplyr::select_(tidy, .dots = list("pie.id", "group", "value"))

    # remove any duplicate patient / group combinations
    tidy <- dplyr::distinct_(tidy, .dots = list("pie.id", "group"))

    # convert the data to wide format
    tidy <- tidyr::spread_(tidy, "group", "value", fill = FALSE, drop = FALSE)

    # join with list of all patients, fill in values of FALSE for any patients
    # not in the data set
    if (!is.null(patients)) {
        tidy <- dplyr::semi_join(tidy, patients, by = "pie.id")
        tidy <- dplyr::mutate_each_(tidy, funs(fill_false), list(quote(-pie.id)))
    }

    tidy
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
#' @param raw.data A data frame with all medications
#' @param ref.data A data frame with three columns: name, type, and group
#' @param sched.data A data frame with all intermittent medications
#'
#' @return A data frame
#'
tidy_meds_cont <- function(raw.data, ref.data, sched.data, patients) {
    # filter to tidy only continuous meds
    ref.data <- dplyr::filter_(ref.data, .dots = list(~group == "cont"))

    # for any med classes, lookup the meds included in the class
    class.meds <- dplyr::filter_(ref.data, .dots = list(~type == "class"))
    class.meds <- med_lookup(class.meds$name)

    # join the list of meds with any indivdual meds included
    lookup.meds <- dplyr::filter_(ref.data, .dots = list(~type == "med"))
    lookup.meds <- c(lookup.meds$name, class.meds$med.name)

    # remove any rows in continuous data which are actually scheduled doses
    tidy <- dplyr::anti_join(raw.data, sched.data, by = "event.id")

    # filter to meds in lookup
    dots <- list(~med %in% lookup.meds)
    tidy <- dplyr::filter_(tidy, .dots = dots)

    # sort by pie.id, med, med.datetime
    tidy <- dplyr::arrange_(tidy, .dots = list("pie.id", "med", "med.datetime"))
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
#' @param raw.data A data frame with all scheduled medications
#' @param ref.data A data frame with three columns: name, type, and group
#'
#' @return A data frame
#'
tidy_meds_sched <- function(raw.data, ref.data) {
    # filter to tidy only scheduled meds
    ref.data <- dplyr::filter_(ref.data, .dots = list(~group == "sched"))

    # for any med classes, lookup the meds included in the class
    class.meds <- dplyr::filter_(ref.data, .dots = list(~type == "class"))
    class.meds <- med_lookup(class.meds$name)

    # join the list of meds with any indivdual meds included
    lookup.meds <- dplyr::filter_(ref.data, .dots = list(~type == "med"))
    lookup.meds <- c(lookup.meds$name, class.meds$med.name)

    # filter to meds in lookup
    dots <- list(~med %in% lookup.meds)
    tidy <- dplyr::filter_(raw.data, .dots = dots)

    # sort by pie.id, med, med.datetime
    tidy <- dplyr::arrange_(tidy, .dots = list("pie.id", "med", "med.datetime"))
}

# function used to compare dates; won't drop POSIXct type
compare_dates <- function(desired, backup) {
    if (is.na(desired)) {
        backup
    } else {
        desired
    }
}

#' Tidy locations
#'
#' \code{tidy_locations} tidy hospital location data
#'
#' This function takes a data frame with hospital location history and produces
#' a tidy version with unit arrival and departure data. It accounts for
#' incorrect departure time from raw EDW data by calculating the departure time
#' using the arrival time of the next unit (unless it was the patient's last
#' unit during the hospitalization). It also combines multiple rows of data when
#' the patient did not actually leave that unit. The data should be read in by
#' \code{\link[BGTools]{read_edw_data}}.
#'
#' @param raw.data A data frame with location data
#'
#' @return A data frame
#'
tidy_locations <- function(raw.data) {
    tidy <- dplyr::group_by_(raw.data, "pie.id")
    tidy <- dplyr::arrange_(tidy, "arrive.datetime")

    # determine if they went to a different unit, then make a count of different
    # units
    dots <- list(~ifelse(is.na(unit.to) | is.na(dplyr::lag(unit.to)) |
                             unit.to != dplyr::lag(unit.to), TRUE, FALSE),
                 ~cumsum(diff.unit))
    nm <- list("diff.unit", "unit.count")
    tidy <- dplyr::mutate_(tidy, .dots = setNames(dots, nm))

    # use the unit.count to group multiple rows of the same unit together and
    # combine data
    dots <- list("pie.id", "unit.count")
    tidy <- dplyr::group_by_(tidy, .dots = dots)

    dots <- list(~dplyr::first(unit.to), ~dplyr::first(arrive.datetime),
                 ~dplyr::last(depart.datetime))
    nm <- list("location", "arrive.datetime", "depart.recorded")
    tidy <- dplyr::summarize_(tidy, .dots = setNames(dots, nm))

    # use the arrival time for the next unit to calculate a depart time
    dots <- list(~dplyr::lead(arrive.datetime))
    nm <- "depart.calculated"
    tidy <- dplyr::mutate_(tidy, .dots = setNames(dots, nm))

    tidy <- dplyr::rowwise(tidy)

    dots <- list(~compare_dates(depart.calculated, depart.recorded),
                 ~as.numeric(difftime(depart.datetime, arrive.datetime,
                                      units = "days")))
    nm <- list("depart.datetime", "unit.length.stay")
    tidy <- dplyr::mutate_(tidy, .dots = setNames(dots, nm))

    dots <- list(quote(-depart.recorded), quote(-depart.calculated))
    tidy <- dplyr::select_(tidy, .dots = dots)

    tidy <- dplyr::ungroup(tidy)
}
