# summarize_data.R

#' Calculate the running time for continuous medication data
#'
#' \code{calc_runtime} calculates the time at current rate and time from start
#'
#' This function takes a data frame with continuous medication rate data and
#' produces a data frame with the time at each rate and the time from start for
#' each row. This could be used to then calculate the AUC or to summarize the
#' continuous data. The data will be grouped into distinct sets of infusions,
#' for patients who may have been restarted on the drip one or more times.
#'
#' @param cont.data A data frame with continuous medication rate data
#' @param drip.off An optional numeric indicating the number of hours a drip
#'   should be off to count as a new infusion, defaults to 12 hours
#' @param no.doc An optional numeric indicating the number of hours without
#'   documentation which will be used to indicate a drip has ended, defaults to
#'   24 hours
#' @param units An optional character string specifying the time units to use in
#'   calculations, default is hours
#'
#' @return A data frame
#'
#' @export
calc_runtime <- function(cont.data, drip.off = 12, no.doc = 24,
                         units = "hours") {
    # group the data by pie.id and med
    cont.data <- dplyr::group_by_(cont.data, .dots = list("pie.id", "med"))

    # determine if it's a valid rate documentation
    dots <- list(~ifelse(is.na(med.rate.units), FALSE, TRUE),
                 ~cumsum(rate.change))
    nm <- list("rate.change", "change.num")
    cont.data <- dplyr::mutate_(cont.data, .dots = setNames(dots, nm))

    # regroup
    cont.data <- dplyr::group_by_(cont.data, "change.num", add = TRUE)

    # fill in missing rates
    dots <- list(~ifelse(is.na(med.rate.units), dplyr::first(med.rate),
                         med.rate))
    nm <- list("rate")
    cont.data <- dplyr::mutate_(cont.data, .dots = setNames(dots, nm))

    # group the data by pie.id and med
    cont.data <- dplyr::group_by_(cont.data, .dots = list("pie.id", "med"))

    # calculate time between rows and order of rate changes
    dots <- list(~as.numeric(difftime(dplyr::lead(med.datetime), med.datetime,
                                      units = units)),
                 ~ifelse(is.na(dplyr::lag(rate)) | rate != dplyr::lag(rate),
                         TRUE, FALSE),
                 ~cumsum(rate.change))
    nm <- list("time.next", "rate.change", "change.num")
    cont.data <- dplyr::mutate_(cont.data, .dots = setNames(dots, nm))

    # regroup
    cont.data <- dplyr::group_by_(cont.data, .dots = list("pie.id", "med",
                                                          "change.num"))

    # calculate how long the drip was at each rate
    dots <- list(~dplyr::first(rate),
                 ~dplyr::first(med.datetime),
                 ~dplyr::last(med.datetime),
                 ~as.numeric(difftime(dplyr::last(med.datetime),
                                      dplyr::first(med.datetime),
                                      units = units)),
                 ~dplyr::last(time.next))
    nm <- list("med.rate", "rate.start", "rate.stop", "rate.duration",
               "time.next")
    cont.data <- dplyr::summarize_(cont.data, .dots = setNames(dots, nm))

    # group the data by pie.id and med
    cont.data <- dplyr::group_by_(cont.data, .dots = list("pie.id", "med"))

    # identify individual drips
    dots <- list(~ifelse(time.next < drip.off & !is.na(time.next),
                         rate.duration + time.next, rate.duration),
                 ~ifelse(is.na(time.next) | time.next > no.doc |
                             (med.rate == 0 & duration > drip.off),
                         TRUE, FALSE),
                 ~ifelse(change.num == 1 | dplyr::lag(drip.stop == TRUE),
                         TRUE, FALSE),
                 ~cumsum(drip.start))
    nm <- list("duration", "drip.stop", "drip.start", "drip.count")
    cont.data <- dplyr::mutate_(cont.data, .dots = setNames(dots, nm))

    # regroup
    cont.data <- dplyr::group_by_(cont.data, "drip.count", add = TRUE)

    # calculate run time
    dots <- list(~as.numeric(difftime(rate.start, first(rate.start),
                                      units = units)))
    nm <- list("run.time")
    cont.data <- dplyr::mutate_(cont.data, .dots = setNames(dots, nm))

    # remove unnecessary columns
    dots <- list(quote(-rate.duration), quote(-time.next), quote(-drip.stop),
                 quote(-drip.start), quote(-change.num))
    cont.data <- dplyr::select_(cont.data, .dots = dots)

    # update drip stop information if rate of last row isn't 0
    dots <- list(~rate.stop == dplyr::last(rate.stop), ~med.rate > 0)
    drip.end <- dplyr::filter_(cont.data, .dots = dots)

    # calculate the run time for the last drip row
    dots <- list(~duration + run.time, "rate.stop", 0)
    nm <- list("run.time", "rate.start", "duration")
    drip.end <- dplyr::mutate_(drip.end, .dots = setNames(dots, nm))

    # bind the rows with drip end data
    cont.data <- dplyr::bind_rows(cont.data, drip.end)

    # regroup
    cont.data <- dplyr::group_by_(cont.data, .dots = list("pie.id", "med",
                                                          "drip.count"))

    # arrange by date/time
    cont.data <- dplyr::arrange_(cont.data, "rate.start")

    return(cont.data)
}

#' Summary calculations for continuous medication data
#'
#' \code{summarize_cont_meds} summarizes continuous medication data
#'
#' This function takes a data frame with continuous medication rate data and
#' produces a data frame with summary data for each patient and medication. The
#' calculations include: first rate, last rate, minimum rate, maximum rate, AUC,
#' time-weighted average rate, total infusion duration, total infusion running
#' time, and cumulative dose.
#'
#' @param cont.data A data frame with continuous medication rate data
#' @param units An optional character string specifying the time units to use in
#'   calculations, default is hours
#'
#' @return A data frame
#'
#' @export
summarize_cont_meds <- function(cont.data, units = "hours") {
    # turn off scientific notation
    options(scipen = 999)

    dots <- list("pie.id", "med", "drip.count")
    cont.data <- dplyr::group_by_(cont.data, .dots = dots)

    # get last and min non-zero rate
    nz.rate <- dplyr::filter_(cont.data, .dots = ~(med.rate > 0))
    dots <- list(~dplyr::last(med.rate), ~min(med.rate, na.rm = TRUE),
                 ~sum(duration, na.rm = TRUE))
    nm <- c("last.rate", "min.rate", "run.time")
    nz.rate <- dplyr::summarize_(nz.rate, .dots = setNames(dots, nm))

    # get first and max rates and AUC
    dots <- list(~dplyr::first(rate.start),
                 ~dplyr::last(rate.stop),
                 ~sum(med.rate * duration, na.rm = TRUE),
                 ~dplyr::first(med.rate),
                 ~max(med.rate, na.rm = TRUE),
                 ~MESS::auc(run.time, med.rate),
                 ~dplyr::last(run.time))
    nm <- c("start.datetime", "stop.datetime", "cum.dose", "first.rate",
            "max.rate", "auc", "duration")
    summary.data <- dplyr::summarize_(cont.data, .dots = setNames(dots, nm))

    # join the last and min data
    summary.data <- dplyr::inner_join(summary.data, nz.rate,
                                      by = c("pie.id", "med", "drip.count"))

    summary.data <- dplyr::ungroup(summary.data)

    # calculate the time-weighted average and interval
    dots <- list(~auc/duration)
    nm <- "time.wt.avg"
    summary.data <- dplyr::mutate_(summary.data, .dots = setNames(dots, nm))

}

#' Calculate run times for labs
#'
#' \code{calc_lab_runtime} calculates run time and duration for labs
#'
#' This function takes a data frame with serial measurement data and produces a
#' data frame with the duration of time at each value and cumulative run time.
#' The data frame should have a column called lab.start which is used as the
#' zero time for calculations.
#'
#' @param cont.data A data frame with serial measurement data
#' @param units An optional string with the unit of measure to pass to
#'   \code{\link{difftime}}, defaults to hours
#'
#' @return A data frame
#'
#' @export
calc_lab_runtime <- function(cont.data, units = "hours") {
    dots <- list(~as.numeric(difftime(lab.datetime, dplyr::lag(lab.datetime),
                                      units = units)),
                 ~ifelse(is.na(duration), 0, duration),
                 ~as.numeric(difftime(lab.datetime, lab.start, units = units)))
    nm <- c("duration", "duration", "run.time")
    cont.data <- dplyr::mutate_(cont.data, .dots = setNames(dots, nm))
}

#' Calculate proportion of time above or below a threshold
#'
#' \code{calc_perc_time} calculates percent time above / below a threshold
#'
#' This function takes a data frame with serial measurement data and produces a
#' data frame with percent time above or below a threshold for each infusion.
#'
#' @param cont.data A data frame with serial measurement data
#' @param thrshld A list of the criteria
#' @param meds An optional logical indicating whether the data contains serial
#'   medication adminstration or laboratory data, defaults to TRUE
#'
#' @return A data frame
#'
#' @examples
#' \dontrun{
#' calc_perc_time(data, list(~med.rate > 0.4))
#' # calculates the proportion of time where the rate is above 0.4
#' }

#' @export
calc_perc_time <- function(cont.data, thrshld, meds = TRUE) {
    # get the total duration of data
    dots <- list(~dplyr::last(run.time))
    nm <- list("total.dur")
    duration <- dplyr::summarize_(cont.data, .dots = setNames(dots, nm))

    # find all values which are within threshold
    goal <- dplyr::filter_(cont.data, .dots = thrshld)

    # calculate the total time at goal
    dots <- list(~sum(duration, na.rm = TRUE))
    nm <- list("time.goal")
    goal <- dplyr::summarize_(goal, .dots = setNames(dots, nm))

    # join the data frames and calculate percent time
    if (meds == TRUE) {
        x <- c("pie.id", "med", "drip.count")
    } else {
        x <- c("pie.id", "lab")
    }

    data <- dplyr::full_join(duration, goal, by = x)

    dots <- list(~ifelse(is.na(time.goal), 0, time.goal),
                 ~time.goal / total.dur)
    nm <- list("time.goal", "perc.time")
    data <- dplyr::mutate_(data, .dots = setNames(dots, nm))

    return(data)
}

#' Summary calculations for lab data
#'
#' \code{summarize_labs} summarizes lab data
#'
#' This function takes a data frame with numeric lab result data and produces a
#' data frame with summary data for each patient and lab. The calculations
#' include: first result, last result, minimum result, maximum result, AUC, and
#' time-weighted average result.
#'
#' @param lab.data A data frame with lab data
#' @param units An optional character string specifying the time units to use in
#'   calculations, default is hours
#'
#' @return A data frame
#'
#' @export
summarize_labs <- function(lab.data, units = "hours") {
    # turn off scientific notation
    options(scipen = 999)

    dots <- list("pie.id", "lab")
    labs <- dplyr::group_by_(lab.data, .dots = dots)

    # get last and min non-zero rate
    # nz.rate <- dplyr::filter_(labs, .dots = ~(med.rate > 0))
    # dots <- list(~dplyr::last(med.rate), ~min(med.rate, na.rm = TRUE),
    #              ~sum(duration, na.rm = TRUE))
    # nm <- c("last.rate", "min.rate", "run.time")
    # nz.rate <- dplyr::summarize_(nz.rate, .dots = setNames(dots, nm))

    # summarize
    dots <- list(~dplyr::first(lab.datetime),
                 ~dplyr::last(lab.datetime),
                 ~dplyr::first(lab.result),
                 ~dplyr::last(lab.result),
                 ~max(lab.result, na.rm = TRUE),
                 ~min(lab.result, na.rm = TRUE),
                 ~MESS::auc(run.time, lab.result))
    nm <- c("first.datetime", "last.datetime", "first.result", "last.result",
            "max.result", "min.result", "auc")
    summary.data <- dplyr::summarize_(labs, .dots = setNames(dots, nm))

    # join the last and min data
    # summary.data <- dplyr::inner_join(summary.data, nz.rate,
    #                                   by = c("pie.id", "med", "drip.count"))

    summary.data <- dplyr::ungroup(summary.data)

    # calculate the time-weighted average and interval
    dots <- list(~auc/duration)
    nm <- "time.wt.avg"
    summary.data <- dplyr::mutate_(summary.data, .dots = setNames(dots, nm))

}

#' Determine if a lab changed by a set amount within a specific time frame
#'
#' \code{lab_change} checks for changes in lab values
#'
#' This function takes a data frame with lab data for a single lab and checks
#' whether the lab changes by a certain amount within a given period of time.
#' The parameters should include: change.by, the threshold which the lab must
#' change by; FUN, the function passed to \code{\link[zoo]{rollapplyr}}; back,
#' the time frame that the lab change must occur in. For FUN, use max when
#' looking for a decrease in lab value, and min when looking for an increase in
#' lab value.
#'
#' @param lab.data A data frame with lab data
#' @param change.by A numeric indicating the threshold for lab changes
#' @param FUN A function for \code{\link[zoo]{rollapplyr}}, most commonly max or
#'   min
#' @param back An optional numeric specifying the number of days back to go.
#'   Defaults to 2 days.
#'
#' @return A data frame
#'
#' @examples
#' \dontrun{
#' lab_change(data, -2, max, back = 2)
#' # checks for a >= 2 decrease in the lab value within the past 2 days
#' }
#'
#' @export
lab_change <- function(lab.data, change.by, FUN, back = 2) {
    lab.data <- dplyr::group_by_(lab.data, .dots = "pie.id")
    lab.data <- dplyr::arrange_(lab.data, .dots = "lab.datetime")

    # calculate the number of rows that are included within the window
    dots <- list(~count_rowsback(lab.datetime, back))
    lab.data <- dplyr::mutate_(lab.data, .dots = setNames(dots, "rowsback"))

    # calculate the running min/max during the time window
    dots <- list(~zoo::rollapplyr(as.numeric(lab.result), rowsback, FUN,
                                  fill = NA, partial = TRUE))
    lab.data <- dplyr::mutate_(lab.data, .dots = setNames(dots, "running"))

    # calcualte the change from the running min/max to current value
    dots <- list(~as.numeric(lab.result) - running)
    lab.data <- dplyr::mutate_(lab.data, .dots = setNames(dots, "change"))

    # filter values which exceed the change.by value
    dots <- list(~abs(change) >= abs(change.by))
    lab.data <- dplyr::filter_(lab.data, .dots = dots)

    return(lab.data)
}
