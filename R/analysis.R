# analysis.R

#' Analysis of data
#'
#' \code{analyze_data} perform data analysis and hypothesis testing
#'
#' This function takes a data frame with data to be analyzed, performs
#' hypothesis testing, and returns a list which can be reported in a markdown
#' document. If my.group is NULL, then the function will look for a column named
#' group in the data frame.
#'
#' @param my.data A data frame with data to be analyzed
#' @param my.group An optional vector to group patients on; defaults to NULL
#'
#' @return A list
#'
#' @export
analyze_data <- function(my.data, my.group = NULL) {
    # look for group column
    if (is.null(my.group)) {
        my.group <- my.data$group
        my.data <- select_(my.data, quote(-group))
    }

    # analyze continuous data
    test <- purrr::keep(my.data, is.numeric)

    ds1 <- lapply(test, tapply, my.group, FSA::Summarize)
    ds2 <- lapply(ds1, do.call, what=cbind)

    err.msg <- "Insufficient sample size for normality testing"

    # a p < 0.05 implies the data is not normally distributed
    sw1 <- lapply(test, function(x)
        tryCatch(shapiro.test(x), error = function(e) err.msg))

    # if not normally distributed, use non-parametric test; if var.test$p.value
    # >= 0.05 then variances are equal
    test_groups <- function(x) {
        if (shapiro.test(x) >= 0.05) {
            wilcox.test(x ~ my.group)
        } else if(var.test(x ~ my.group)$p.value >= 0.05) {
            t.test(x ~ my.group)
        } else {
            t.test(x ~ my.group, var.equal=TRUE)
        }
    }

    err.msg <- "Insufficient sample size for inference testing"

    tt3 <- lapply(test, function(x)
        tryCatch(test_groups(x), error = function(e) err.msg))

    ds3 <- lapply(seq_along(ds1), function(i) list(results = ds2[[i]],
                                                   normality = sw1[[i]],
                                                   comparison = tt3[[i]]))

    names(ds3) <- names(ds1)

    cont.data <- ds3

    # analyze categorical data
    test <- purrr::discard(my.data, is.numeric)

    ds1 <- lapply(test, function(x) table(x, my.group))
    ds2 <- lapply(test, function(x) FSA::Summarize(my.group ~ x,
                                                   percent = "column",
                                                   addtotal = FALSE))

    err.msg <- "Chi-squared could not be performed"

    csq <- lapply(test, function(x)
        tryCatch(chisq.test(x, my.group), error = function(e) err.msg))

    ds3 <- lapply(seq_along(ds1), function(i) list(counts = ds1[[i]],
                                                   percents = ds2[[i]],
                                                   chi.sq = csq[[i]]))
    names(ds3) <- names(ds1)

    cat.data <- ds3

    return(c(cont.data, cat.data))
}
