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
#'
#' @param data.dir A character string with the name of the directory containing
#'   the data files
#' @param file.name A character string with name of data file or pattern to
#'   match
#' @param type An optional character string indicating type of data being tidied
#'
#' @return A data frame (tbl_df)
#'
#' @seealso \code{\link[readr]{read_csv}}
#'
#' @export
read_edw_data <- function(data.dir, file.name, type = NA) {
    # if type is NA, then set type to file.name
    if (is.na(type)) {
        type <- file.name
    }

    # get list of files is specified directory and matching file name
    raw <- list.files(data.dir, pattern = file.name, full.names = TRUE)

    exclude <- c("", "Unknown")
    pt.id <- "pie.id"
    dt.format <- "%Y/%m/%d %H:%M:%S"
    dots <- NULL
    nm <- NULL

    col_character <- readr::col_character
    col_datetime <- readr::col_datetime
    col_double <- readr::col_double
    col_factor <- readr::col_factor
    col_integer <- readr::col_integer
    col_skip <- readr::col_skip

    switch(type,
           admit_dc = {
               col_names <- c(pt.id, "arrival.datetime", "admit.datetime",
                       "discharge.datetime")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_datetime(format = dt.format),
                                        col_datetime(format = dt.format))
           },

           blood = {
               col_names <- c(pt.id, "blood.datetime", "blood.prod",
                              "blood.type")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_character())
               dots <- list(~assign_blood_prod(stringr::str_to_lower(blood.prod)))
               nm <- "blood.prod"
           },

           charges = {
               col_names <- c(pt.id, "cdm.code", "service.date", "institution")
               col_types <- readr::cols(col_character(),
                                        col_character(),
                                        col_skip(),
                                        col_datetime(format = dt.format),
                                        col_skip(),
                                        col_character())
           },

           demographics = {
               col_names <- c(pt.id, "age", "sex", "race", "disposition",
                              "length.stay", "visit.type", "person.id",
                              "facility")
               race <- c("African American", "Asian", "Latin American",
                         "Native Am.", "Other", "White/Caucasian")
               col_types <- readr::cols(col_character(),
                                        col_integer(),
                                        col_factor(c("Female", "Male")),
                                        col_factor(race),
                                        col_character(),
                                        col_double(),
                                        col_character(),
                                        col_character(),
                                        col_character())
               dots <- list(~factor(disposition, exclude = exclude),
                            ~factor(visit.type, exclude = exclude),
                            ~factor(facility, exclude = exclude))
               nm <- c("disposition", "visit.type", "facility")
           },

           diagnosis = {
               col_names <- c(pt.id, "diag.code", "diag.type", "diag.seq")
               col_types <- readr::cols(col_character(),
                                        col_character(),
                                        col_character(),
                                        col_factor(c(0, 1, 99)))
               dots <- list(~factor(diag.type, exclude = exclude))
               nm <- "diag.type"
           },

           encounters = {
               col_names <- c("person.id", "admit.datetime", pt.id,
                              "visit.type", "facility", "disposition")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_character())
               dots <- list(~factor(visit.type, exclude = exclude),
                            ~factor(facility, exclude = exclude),
                            ~factor(disposition, exclude = exclude))
               nm <- c("visit.type", "facility", "disposition")
           },

           events = {
               col_names <- c(pt.id, "event.datetime", "event", "event.result")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_character())
               dots <- list(~stringr::str_to_lower(event))
               nm <- "event"
           },

           home_meds = {
               col_names <- c(pt.id, "med", "order.name", "med.type")
               col_types <- readr::cols(col_character(),
                                        col_character(),
                                        col_character(),
                                        col_character())
               dots <- list(~stringr::str_to_lower(med),
                            ~factor(med.type))
               nm <- c("med", "med.type")
           },

           id = {
               col_names <- c(pt.id, "fin", "person.id")
               col_types <- readr::cols(col_character(),
                                        col_character(),
                                        col_character())
           },

           icu_assess = {
               col_names <- c(pt.id, "assess.datetime", "assessment", "assess.result")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_character())
               dots <- list(~stringr::str_to_lower(assessment))
               nm <- "assessment"
           },

           labs = {
               col_names <- c(pt.id, "lab.datetime", "lab", "lab.result")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_character())
               dots <- list(~stringr::str_to_lower(lab))
               nm <- "lab"
           },

           locations = {
               col_names <- c(pt.id, "unit.from", "unit.to", "arrive.datetime",
                              "depart.datetime")
               col_types <- readr::cols(col_character(),
                                        col_character(),
                                        col_character(),
                                        col_datetime(format = dt.format),
                                        col_datetime(format = dt.format))
           },

           measures = {
               col_names <- c(pt.id, "measure.datetime", "measure",
                              "measure.result", "measure.units")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_factor(c("Height", "Weight")),
                                        col_double(),
                                        col_factor(c("cm", "in", "lb", "kg")))
           },

           meds_continuous = {
               col_names <- c(pt.id, "med.datetime", "med", "med.rate",
                              "med.rate.units", "event.id")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_double(),
                                        col_character(),
                                        col_character())
               dots <- list(~stringr::str_to_lower(med),
                            ~factor(med.rate.units, exclude = exclude))
               nm <- c("med", "med.rate.units")
           },

           meds_sched = {
               col_names <- c(pt.id, "med.datetime", "med", "med.dose",
                              "med.dose.units", "med.route", "event.id")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_double(),
                                        col_character(),
                                        col_character(),
                                        col_skip(),
                                        col_character())
               dots <- list(~stringr::str_to_lower(med),
                            ~factor(med.dose.units, exclude = exclude),
                            ~factor(med.route, exclude = exclude))
               nm <- c("med", "med.dose.units", "med.route")
           },

           meds_sched_freq = {
               col_names <- c(pt.id, "med.datetime", "med", "med.dose",
                              "med.dose.units", "med.route", "event.id")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_double(),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_character())
               dots <- list(~stringr::str_to_lower(med),
                            ~factor(med.dose.units, exclude = exclude),
                            ~factor(med.route, exclude = exclude),
                            ~factor(freq, exclude = exclude))
               nm <- c("med", "med.dose.units", "med.route", "freq")
           },

           mpp = {
               col_names <- c(pt.id, "mpp")
               col_types <- readr::cols(col_character(), col_character())
           },

           problems = {
               col_names <- c(pt.id, "problem", "classification", "confirm",
                              "free.text", "severity", "active",
                              "onset.datetime", "life.cycle",
                              "life.cycle.datetime")
               col_types <- readr::cols(col_character(),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_datetime(format = dt.format))
               dots <- list(~factor(classification, exclude = exclude),
                            ~factor(confirm, exclude = exclude),
                            ~factor(free.text, exclude = exclude),
                            ~factor(severity, exclude = exclude),
                            ~factor(active, exclude = exclude),
                            ~factor(life.cycle, exclude = exclude))
               nm <- c("classification", "confirm", "free.text", "severity",
                       "active", "life.cycle")
           },

           procedures = {
               col_names <- c(pt.id, "proc.date", "proc.code")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character())
           },

           radiology = {
               col_names <- c(pt.id, "rad.datetime", "rad.type")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character())
           },

           surgeries = {
               col_names <- c(pt.id, "surg.start.datetime",
                              "surg.stop.datetime", "surgery", "add.on",
                              "asa.class", "primary.proc")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_character())
               dots <- list(~ifelse(add.on == 1, TRUE, FALSE),
                            ~factor(asa.class, exclude = exclude),
                            ~ifelse(primary.proc == 1, TRUE, FALSE))
               nm <- c("add.on", "asa.class", "primary.proc")
           },

           uop = {
               col_names <- c(pt.id, "uop.datetime", "uop.event", "uop.result")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_character())
               dots <- list(~stringr::str_to_lower(uop.event))
               nm <- "uop.event"
           },

           vent = {
               col_names <- c(pt.id, "vent.datetime", "vent.event", "vent.result")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_character())
               dots <- list(~stringr::str_to_lower(vent.event))
               nm <- "vent.event"
           },

           vitals = {
               col_names <- c(pt.id, "vital.datetime", "vital", "vital.result")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_character())
               dots <- list(~stringr::str_to_lower(vital))
               nm <- "vital"
           },

           warfarin = {
               col_names <- c(pt.id, "warfarin.datetime", "warfarin.event",
                              "warfarin.result")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_character())
               dots <- list(~stringr::str_to_lower(warfarin.event))
               nm <- "warfarin.event"
           },

           stop("Invalid type")

    )

    # loop through all matching files and read in to list take list of files and
    # bind them together into a data frame; will set column names based on the
    # type of data, so skip the header row
    read <- purrr::map_df(raw, readr::read_csv, col_names = col_names,
                          col_types = col_types, skip = 1)
    read <- dplyr::distinct_(read)

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
