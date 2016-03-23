# read_data.R

#' Read and join data from multiple csv files
#'
#' \code{read_data} takes a directory and file name and reads in all matching
#' csv files and binds them together into a data frame using the base function
#' \code{\link{read.csv}}.
#'
#' This function takes a directory and file name and reads in all matching csv
#' files and binds them together into a data frame.
#'
#' @param data.dir A character string with the name of the directory containing
#'   the data files
#' @param file.name A character string with name of data file or pattern to
#'   match
#'
#' @return A data frame
#'
#' @seealso \code{\link{read.csv}}
#'
#' @export
read_data <- function(data.dir, file.name) {
    # get list of files is specified directory and matching file name
    raw <- list.files(data.dir, pattern = file.name, full.names = TRUE)
    # loop through all matching files and read in to list take list of files and
    # bind them together into a data frame
    raw <- purrr::map_df(raw, read.csv, colClasses = "character")

    # remove any duplicate rows
    raw <- dplyr::distinct_(raw)
}

#' Read EDW data from csv files
#'
#' \code{read_edw_data} takes a directory and file name and reads in all
#' matching csv files and binds them together into a data frame
#'
#' This function takes a directory and file name and reads in all matching csv
#' files and binds them together into a data frame using
#' \code{\link[readr]{read_csv}} from the readr package.
#'
#' Valid options for type include: admit_dc, blood, charges, demographisc,
#' diagnosis, encounters, events, home_meds, icu_assess, id, labs, locations,
#' measures, meds_continuous, meds_sched, meds_sched_freq, mpp, problems,
#' procedures, radiology, surgeries, uop, vent, vitals, warfarin
#'
#' @param data.dir A character string with the name of the directory containing
#'   the data files
#' @param file.name A character string with name of data file or pattern to
#'   match
#' @param type An optional character string indicating type of data being tidied
#' @param check.distinct An optional logical, calls
#'   \code{\link[dplyr]{distinct}} on the imported data if \code{TRUE}
#'
#' @return A data frame (tbl_df)
#'
#' @seealso \code{\link[readr]{read_csv}}
#'
#' @export
read_edw_data <- function(data.dir, file.name, type = NA,
                          check.distinct = FALSE) {
    # if type is NA, then set type to file.name
    if (is.na(type)) {
        type <- file.name
    }

    # get list of files is specified directory and matching file name
    raw <- list.files(data.dir, pattern = file.name, full.names = TRUE)

    exclude <- c("", "Unknown")
    raw.names <- list(id = "PowerInsight Encounter Id",
                      dt = "Clinical Event End Date/Time",
                      ev = "Clinical Event",
                      res = "Clinical Event Result")
    pt.id <- "pie.id"
    dots <- NULL
    nm <- NULL

    col_dt <- readr::col_datetime(format = "%Y/%m/%d %H:%M:%S")

    # set defaults which are used in multiple types
    col.raw <- c(raw.names$id, raw.names$dt, raw.names$ev, raw.names$res)
    col.names <- c(pt.id, "event.datetime", "event", "result")
    col.types <- readr::cols_only("c", col_dt, "c", "c")

    switch(type,
           admit_dc = {
               col.raw <- c(raw.names$id, "Arrival Date & Time",
                            "Admit Date & Time", "Discharge Date & Time")
               col.names <- c(pt.id, "arrival.datetime", "admit.datetime",
                       "discharge.datetime")
               col.types <- readr::cols("c", col_dt, col_dt, col_dt)
           },

           blood = {
               # use default columns
               col.names <- c(pt.id, "blood.datetime", "blood.prod",
                              "blood.type")
               dots <- list(~assign_blood_prod(stringr::str_to_lower(blood.prod)))
               nm <- "blood.prod"
           },

           charges = {
               col.raw <- c(raw.names$id, "Cdm Code", "Service Date",
                            "Institution Desc")
               col.names <- c(pt.id, "cdm.code", "service.date", "institution")
               col.types <- readr::cols_only("c", "c", col_dt, "c")
           },

           demographics = {
               col.raw <- c(raw.names$id, "Age- Years (Visit)", "Sex", "Race",
                            "Discharge Disposition", "LOS (Actual)",
                            "Encounter Type", "Person ID",
                            "Person Location- Facility (Curr)")
               col.names <- c(pt.id, "age", "sex", "race", "disposition",
                              "length.stay", "visit.type", "person.id",
                              "facility")
               race <- c("African American", "Asian", "Latin American",
                         "Native Am.", "Other", "White/Caucasian")
               col.types <- readr::cols("c", "i",
                                        readr::col_factor(c("Female", "Male")),
                                        readr::col_factor(race),
                                        "c", "d", "c", "c", "c")
               dots <- list(~factor(disposition, exclude = exclude),
                            ~factor(visit.type, exclude = exclude),
                            ~factor(facility, exclude = exclude))
               nm <- c("disposition", "visit.type", "facility")
           },

           diagnosis = {
               col.raw <- c(raw.names$id, "ICD9 Diagnosis Code",
                            "Diagnosis Type", "Diagnosis Code Sequence")
               col.names <- c(pt.id, "diag.code", "diag.type", "diag.seq")
               col.types <- readr::cols("c", "c", "c", "c")
           },

           encounters = {
               col.raw <- c("Person ID", "Admit Date & Time", raw.names$id,
                            "Encounter Type",
                            "Person Location - Facility (Admit)",
                            "Discharge Disposition")
               col.names <- c("person.id", "admit.datetime", pt.id,
                              "visit.type", "facility", "disposition")
               col.types <- readr::cols("c", col_dt, "c", "c", "c", "c")
               dots <- list(~factor(visit.type, exclude = exclude),
                            ~factor(facility, exclude = exclude),
                            ~factor(disposition, exclude = exclude))
               nm <- c("visit.type", "facility", "disposition")
           },

           events = {
               # use default columns
               col.names <- c(pt.id, "event.datetime", "event", "event.result")
               dots <- list(~stringr::str_to_lower(event))
               nm <- "event"
           },

           home_meds = {
               col.raw <- c(raw.names$id, "Orig Orderable Type-Flag Desc",
                            "Order Catalog Short Description",
                            "Order Catalog Mnemonic")
               col.names <- c(pt.id, "med", "order.name", "med.type")
               col.types <- readr::cols("c", "c", "c", "c")
               dots <- list(~stringr::str_to_lower(med))
               nm <- "med"
           },

           icu_assess = {
               # use default columns
               col.names <- c(pt.id, "assess.datetime", "assessment",
                              "assess.result")
               dots <- list(~stringr::str_to_lower(assessment))
               nm <- "assessment"
           },

           id = {
               col.raw <- c(raw.names$id, "Formatted Financial Nbr",
                            "Person ID")
               col.names <- c(pt.id, "fin", "person.id")
               col.types <- readr::cols("c", "c", "c")
           },

           labs = {
               # use default columns
               col.names <- c(pt.id, "lab.datetime", "lab", "lab.result")
               dots <- list(~stringr::str_to_lower(lab))
               nm <- "lab"
           },

           locations = {
               col.raw <- c(raw.names$id, "Location Arrival Date & Time",
                            "Location Depart Date & Time",
                            "Person Location - Nurse Unit (To)",
                            "Person Location - Nurse Unit (From)")
               col.names <- c(pt.id, "arrive.datetime", "depart.datetime",
                              "unit.to", "unit.from")
               col.types <- readr::cols("c", col_dt, col_dt, "c", "c")
           },

           measures = {
               col.raw <- c(raw.names$id, raw.names$dt, raw.names$ev,
                            raw.names$res, "Clinical Event Result Units")
               col.names <- c(pt.id, "measure.datetime", "measure",
                              "measure.result", "measure.units")
               col.types <- readr::cols("c", col_dt, "c", "d", "c")
           },

           meds_continuous = {
               col.raw <- c(raw.names$id, raw.names$dt, raw.names$ev,
                               "Infusion Rate", "Infusion Rate Unit",
                               "Event ID")
               col.names <- c(pt.id, "med.datetime", "med", "med.rate",
                              "med.rate.units", "event.id")
               col.types <- readr::cols("c", col_dt, "c", "d", "c", "c")
               dots <- list(~stringr::str_to_lower(med))
               nm <- "med"
           },

           meds_sched = {
               col.raw <- c(raw.names$id, raw.names$dt, raw.names$ev,
                            "Dosage Amount", "Dosage Unit",
                            "Route of Administration - Short",
                            "Event ID")
               col.names <- c(pt.id, "med.datetime", "med", "med.dose",
                              "med.dose.units", "med.route", "event.id")
               col.types <- readr::cols_only("c", col_dt, "c", "d", "c", "c",
                                             "c")
               dots <- list(~stringr::str_to_lower(med))
               nm <- "med"
           },

           meds_sched_freq = {
               col.raw <- c(raw.names$id, raw.names$dt, raw.names$ev,
                            "Dosage Amount", "Dosage Unit",
                            "Route of Administration - Short",
                            "Parent Order Frequency Description","Event ID")
               col.names <- c(pt.id, "med.datetime", "med", "med.dose",
                              "med.dose.units", "med.route", "freq", "event.id")
               col.types <- readr::cols_only("c", col_dt, "c", "d", "c", "c",
                                             "c", "c")
               dots <- list(~stringr::str_to_lower(med))
               nm <- "med"
           },

           mpp = {
               col.raw <- c(raw.names$id, "MPP (which generated order)")
               col.names <- c(pt.id, "mpp")
               col.types <- readr::cols("c", "c")
           },

           problems = {
               col.raw <- c(raw.names$id, "Problem - Description",
                            "Problem Classification",
                            "Problem Confirmation Status",
                            "Problem Free Text", "Problem Severity",
                            "Problem Source Active Indicator",
                            "Problem Onset Date & Time",
                            "Problem Life Cycle Date & Time",
                            "Problem Life Cycle")
               col.names <- c(pt.id, "problem", "classification", "confirm",
                              "free.text", "severity", "active",
                              "onset.datetime", "life.cycle",
                              "life.cycle.datetime")
               col.types <- readr::cols("c", "c", "c", "c", "c", "c", "c",
                                        col_dt, col_dt, "c")
           },

           procedures = {
               col.raw <- c(raw.names$id, "Procedure Date and Time",
                            "ICD9 Procedure Code")
               col.names <- c(pt.id, "proc.date", "proc.code")
               col.types <- readr::cols("c", col_dt, "c")
           },

           radiology = {
               col.raw <- c(raw.names$id, raw.names$dt, raw.names$ev)
               col.names <- c(pt.id, "rad.datetime", "rad.type")
               col.types <- readr::cols("c", col_dt, "c")
           },

           surgeries = {
               col.raw <- c(raw.names$id, "Priority", "ASA Class Description",
                            "Add On Indicator", "Procedure",
                            "Primary Procedure Indicator", "Start Date/Time",
                            "Stop Date/Time")
               col.names <- c(pt.id, "priority", "asa.class", "add.on",
                              "surgery", "primary.proc", "surg.start.datetime",
                              "surg.stop.datetime")
               col.types <- readr::cols_only("c", "c", "c", "c", "c", "c",
                                             col_dt, col_dt)
               dots <- list(~ifelse(add.on == 1, TRUE, FALSE),
                            ~factor(asa.class, exclude = exclude),
                            ~ifelse(primary.proc == 1, TRUE, FALSE))
               nm <- c("add.on", "asa.class", "primary.proc")
           },

           uop = {
               # use default columns
               col.names <- c(pt.id, "uop.datetime", "uop.event", "uop.result")
               dots <- list(~stringr::str_to_lower(uop.event))
               nm <- "uop.event"
           },

           vent = {
               # use default columns
               col.names <- c(pt.id, "vent.datetime", "vent.event", "vent.result")
               dots <- list(~stringr::str_to_lower(vent.event))
               nm <- "vent.event"
           },

           vitals = {
               # use default columns
               col.names <- c(pt.id, "vital.datetime", "vital", "vital.result")
               dots <- list(~stringr::str_to_lower(vital))
               nm <- "vital"
           },

           warfarin = {
               # use default columns
               col.names <- c(pt.id, "warfarin.datetime", "warfarin.event",
                              "warfarin.result")
               dots <- list(~stringr::str_to_lower(warfarin.event))
               nm <- "warfarin.event"
           },

           stop("Invalid type")

    )

    # sets column names to be read in; read all matching files into a single
    # data_frame
    col.types$cols <- purrr::set_names(col.types$cols, col.raw)
    read <- purrr::map_df(raw, readr::read_csv, col_types = col.types)

    # convert vector of columns to a list of symbols which work with dplyr, then
    # rename columns to more convenient names
    col.raw <- purrr::map(col.raw, as.symbol)
    read <- dplyr::rename_(read, .dots = setNames(col.raw, col.names))

    # indicator to remove duplicates; defaults to FALSE for faster read times
    if (check.distinct == TRUE) {
        read <- dplyr::distinct_(read)
    }

    # perform any necessary data mutations
    if (!is.null(dots)) {
        read <- dplyr::mutate_(read, .dots = setNames(dots, nm))
    }

    read
}

# evaluate clinical event and determine which blood product was given
assign_blood_prod <- function(event) {

    # sub-function to evaluate each row
    get_prod <- function(x) {
        if(stringr::str_detect(x, "cryo")) {
            prod <- "cryo"
        } else if(stringr::str_detect(x, "ffp")) {
            prod <- "ffp"
        } else if(stringr::str_detect(x, "rbc")) {
            prod <- "prbc"
        } else if(stringr::str_detect(x, "platelet")) {
            prod <- "platelet"
        } else {
            prod <- "unknown"
        }
    }

    # loop through each element of the vector; returns a new vector with a
    # string identifying the product type for each element in the original
    # vector
    purrr::map_chr(event, get_prod)
}