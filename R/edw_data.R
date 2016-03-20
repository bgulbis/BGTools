# edw_data.R

#' Read EDW data from csv files
#'
#' \code{read_edw_data} takes a directory and file name and reads in all
#' matching csv files and binds them together into a data frame
#'
#' This function takes a directory and file name and reads in all
#' matching csv files and binds them together into a data frame.
#'
#'
#' @param data.dir A character string with the name of the directory containing the
#'   data files
#' @param file.name A character string with name of data file or pattern to match
#' @param type An optional character string indicating type of data being tidied
#'
#' @return A data frame (tbl_df)
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
               col_names <- c(pt.id, "arrive.datetime", "depart.datetime",
                              "unit.to", "unit.from")
               col_types <- readr::cols(col_character(),
                                        col_datetime(format = dt.format),
                                        col_datetime(format = dt.format),
                                        col_character(),
                                        col_character())
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

#' Read and join data from multiple csv files
#'
#' \code{read_data} takes a directory and file name and reads in all matching
#' csv files and binds them together into a data frame
#'
#' This function takes a directory and file name and reads in all matching csv
#' files and binds them together into a data frame.
#'
#' @param data.dir A character string with the name of the directory containing the
#'   data files
#' @param file.name A character string with name of data file or pattern to match
#'
#' @return A data frame
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

#' Tidy data from EDW
#'
#' \code{tidy_edw_data} takes a data frame with raw data from EDW and returns a
#' tidied data frame
#'
#' This function takes a data frame with raw data generated by an EDW query, and
#' a character vector indicating the type of data being tidied, and will return
#' a tidied data frame. Valid options for type include: admit_dc, blood,
#' demographics, diagnosis, encounters, home_meds, icu_assess, id, labs,
#' location, measures, meds_continuous, meds_freq, meds_sched, meds_sched_freq,
#' mpp, problems, procedures, surgeries, radiology, uop, vent, vitals, warfarin.
#'
#'
#' @param raw.data A data frame with raw data from EDW
#' @param type A character vector indicating the type of data being tidied
#'
#' @return A data frame
#'
tidy_edw_data <- function(raw.data, type) {

    # set field modifications (dots) and names of columns (nm)
    switch(type,
           admit_dc = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Arrival.Date...Time),
                            ~lubridate::ymd_hms(Admit.Date...Time),
                            ~lubridate::ymd_hms(Discharge.Date...Time))
               nm <- c("pie.id", "arrival.datetime", "admit.datetime",
                       "discharge.datetime")
           },

           blood = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Clinical.Event.End.Date.Time),
                            ~assign_blood_prod(stringr::str_to_lower(Clinical.Event)),
                            "Clinical.Event.Result")
               nm <- c("pie.id", "blood.datetime", "blood.prod", "blood.type")
           },

           demographics = {
               dots <- list("PowerInsight.Encounter.Id",
                            "Person.ID",
                            ~as.numeric(Age..Years..Visit.),
                            ~factor(Sex, exclude = c("", "Unknown")),
                            ~factor(Race, exclude = c("", "Unknown")),
                            ~factor(Discharge.Disposition),
                            ~as.numeric(LOS..Actual.),
                            ~factor(Person.Location..Facility..Curr., exclude = ""),
                            ~factor(Encounter.Type))
               nm <- c("pie.id", "person.id", "age", "sex", "race", "disposition",
                       "los", "facility", "visit.type")
           },

           diagnosis = {
               dots <- list("PowerInsight.Encounter.Id",
                            "ICD9.Diagnosis.Code",
                            ~factor(Diagnosis.Type, exclude = ""),
                            ~factor(Diagnosis.Code.Sequence))
               nm <- c("pie.id", "diag.code", "diag.type", "diag.seq")
           },

           encounters = {
               dots <- list("Person.ID",
                            "PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Admit.Date...Time),
                            ~factor(Encounter.Type, exclude = ""),
                            ~factor(Person.Location...Facility..Admit., exclude = ""),
                            ~factor(Discharge.Disposition, exclude = ""))
               nm <- c("person.id", "pie.id", "admit.datetime", "encounter.type",
                       "location", "disposition")
           },

           home_meds = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~stringr::str_to_lower(Order.Catalog.Short.Description),
                            "Order.Catalog.Mnemonic",
                            ~factor(Orig.Orderable.Type.Flag.Desc))
               nm <- c("pie.id", "med", "order.name", "med.type")
           },

           id = {
               dots <- list("PowerInsight.Encounter.Id",
                            "Formatted.Financial.Nbr",
                            "Person.ID")
               nm <- c("pie.id", "fin", "person.id")
           },

           icu_assess = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Clinical.Event.End.Date.Time),
                            ~stringr::str_to_lower(Clinical.Event),
                            "Clinical.Event.Result")
               nm <- c("pie.id", "assess.datetime", "assessment", "assess.result")
           },

           labs = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Clinical.Event.End.Date.Time),
                            ~stringr::str_to_lower(Clinical.Event),
                            "Clinical.Event.Result")
               nm <- c("pie.id", "lab.datetime", "lab", "lab.result")
           },

           locations = {
               dots <- list("PowerInsight.Encounter.Id",
                            "Person.Location...Nurse.Unit..From.",
                            "Person.Location...Nurse.Unit..To.",
                            ~lubridate::ymd_hms(Location.Arrival.Date...Time),
                            ~lubridate::ymd_hms(Location.Depart.Date...Time))
               nm <- c("pie.id", "unit.from", "unit.to", "arrive.datetime",
                       "depart.datetime")
           },

           measures = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Clinical.Event.End.Date.Time),
                            ~stringr::str_to_lower(Clinical.Event),
                            ~as.numeric(Clinical.Event.Result),
                            ~factor(Clinical.Event.Result.Units))
               nm <- c("pie.id", "measure.datetime", "measure", "measure.result",
                       "measure.units")
           },

           meds_continuous = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Clinical.Event.End.Date.Time),
                            ~stringr::str_to_lower(Clinical.Event),
                            ~as.numeric(Infusion.Rate),
                            ~factor(Infusion.Rate.Unit, exclude = ""),
                            "Event.ID")
               nm <- c("pie.id", "med.datetime", "med", "med.rate", "med.rate.units",
                       "event.id")
           },

           meds_sched = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Clinical.Event.End.Date.Time),
                            ~stringr::str_to_lower(Clinical.Event),
                            ~as.numeric(Dosage.Amount),
                            ~factor(Dosage.Unit, exclude = ""),
                            ~factor(Route.of.Administration...Short, exclude = ""),
                            "Event.ID")
               nm <- c("pie.id", "med.datetime", "med", "med.dose", "med.dose.units",
                       "med.route", "event.id")
           },

           meds_sched_freq = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Clinical.Event.End.Date.Time),
                            ~stringr::str_to_lower(Clinical.Event),
                            ~as.numeric(Dosage.Amount),
                            ~factor(Dosage.Unit, exclude = ""),
                            ~factor(Route.of.Administration...Short, exclude = ""),
                            "Parent.Order.Frequency.Description",
                            "Event.ID")
               nm <- c("pie.id", "med.datetime", "med", "med.dose", "med.dose.units",
                       "med.route", "freq", "event.id")
           },

           problems = {
               dots <- list("PowerInsight.Encounter.Id",
                            "Problem...Description",
                            ~factor(Problem.Classification, exclude = ""),
                            ~factor(Problem.Confirmation.Status, exclude = ""),
                            ~factor(Problem.Free.Text, exclude = ""),
                            ~factor(Problem.Severity, exclude = ""),
                            ~factor(Problem.Source.Active.Indicator, exclude = ""),
                            ~lubridate::ymd_hms(Problem.Onset.Date...Time),
                            ~factor(Problem.Life.Cycle, exclude = ""),
                            ~lubridate::ymd_hms(Problem.Life.Cycle.Date...Time))
               nm <- c("pie.id", "problem", "classification", "confirm", "free.text",
                       "severity", "active", "onset.datetime", "life.cycle",
                       "life.cycle.datetime")
           },

           procedures = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Procedure.Date.and.Time),
                            "ICD9.Procedure.Code")
               nm <- c("pie.id", "proc.date", "proc.code")
           },

           radiology = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Clinical.Event.End.Date.Time),
                            "Clinical.Event")
               nm <- c("pie.id", "rad.datetime", "rad.type")
           },

           surgeries = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Start.Date.Time),
                            ~lubridate::ymd_hms(Stop.Date.Time),
                            "Procedure",
                            ~ifelse(Add.On.Indicator == 1, TRUE, FALSE),
                            ~factor(ASA.Class, exclude = ""),
                            ~ifelse(Primary.Procedure.Indicator == 1, TRUE, FALSE))
               nm <- c("pie.id", "surg.start.datetime", "surg.stop.datetime",
                       "surgery", "add.on", "asa.class", "primary.proc")
           },

           uop = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Clinical.Event.End.Date.Time),
                            ~stringr::str_to_lower(Clinical.Event),
                            "Clinical.Event.Result")
               nm <- c("pie.id", "uop.datetime", "uop.event", "uop.result")
           },

           vent = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Clinical.Event.End.Date.Time),
                            ~stringr::str_to_lower(Clinical.Event),
                            "Clinical.Event.Result")
               nm <- c("pie.id", "vent.datetime", "vent.event", "vent.result")
           },

           vitals = {
               dots <- list("PowerInsight.Encounter.Id",
                            ~lubridate::ymd_hms(Clinical.Event.End.Date.Time),
                            ~stringr::str_to_lower(Clinical.Event),
                            "Clinical.Event.Result")
               nm <- c("pie.id", "vital.datetime", "vital", "vital.result")
           },

           stop("Invalid type")
    )

    # apply the parameters to the columns, remove any remaining columns
    tidy.data <- dplyr::transmute_(raw.data, .dots = setNames(dots, nm))

    return(tidy.data)
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
    sapply(event, get_prod)
}
