# warfarin_goals.R

#' Make INR therapeutic ranges
#'
#' \code{make_inr_ranges} tidy INR ranges data
#'
#' This function takes a data frame with the goal INR range as input into the
#' EMR and returns a tidy data frame with the low and high goal values. It
#' tidies up string errors (typos, incorrect ranges, etc.) before returning the
#' values.
#'
#' @param raw.data A data frame with INR ranges
#'
#' @return A data frame
#'
#' @export
make_inr_ranges <- function(raw.data) {
    # make sure we are only working with INR range data, remove and empty values
    dots <- list(~warfarin.event == "inr range", ~warfarin.result != "")
    tidy <- dplyr::filter_(raw.data, .dots = dots)

    fix_ranges <- function(x, y) {
        tidy <<- purrr::dmap_at(.d = tidy, .at = "warfarin.result",
                               .f = stringr::str_replace_all,
                               pattern = stringr::regex(x, ignore_case = TRUE),
                               replacement = y)
    }

    find <- c("(INR|Goal)|-\\.|\\(.*\\)|=",
              "above|greater( than)?",
              "below|less( than)?",
              "\\.\\.",
              "--|to|/",
              "[0-9\\.]+( )[0-9\\.]+",
              "[1-9\\.]+([0])[1-9\\.]+")

    replace <- c("", ">", "<", ".", "-", "-", "-")

    # perform string replacements to clean up inr ranges
    purrr::walk2(.x = find, .y = replace, .f = fix_ranges)

    # separate the inr range into two columns, goal low and high
    tidy <- tidyr::extract_(data = tidy, col = "warfarin.result",
                            into = c("goal.low", "goal.high"),
                            regex = "([0-9\\.]+ ?)-( ?[0-9\\.]+)",
                            remove = FALSE, convert = TRUE)

    # correct any goals like "200", which should be "2.0", or "25" = "2.5"
    fix_div <- function(y, n) {
        purrr::as_vector(purrr::map_if(y, ~ !is.na(.x) && .x >= n, ~ .x / n))
    }

    tidy <- purrr::dmap_at(.d = tidy, .at = c("goal.low", "goal.high"),
                           .f = fix_div, n = 100)
    tidy <- purrr::dmap_at(.d = tidy, .at = c("goal.low", "goal.high"),
                           .f = fix_div, n = 10)

    tidy
}
