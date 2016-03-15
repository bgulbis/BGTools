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
    cont <- sapply(my.data, is.numeric)
    test <- my.data[, c(cont)]

    ds1 <- lapply(test, tapply, my.group, Summarize)
    ds2 <- lapply(ds1, do.call, what=cbind)

    # a p < 0.05 implies the data is not normally distributed
    sw1 <- lapply(test, function(x)
        if (sum(!is.na(x)) >= 3) {
            shapiro.test(x)
        } else {
            "Insufficient sample size for normality testing"
        })

    # lapply(test, function(x) str(split(x, my.group)))
    # if not normally distributed, use non-parametric test
    # if var.test$p.value >= 0.05 then variances are equal
    tt3 <- lapply(test, function(x)
        if(sum(!is.na(x)) >= 3 & sum(!duplicated(my.group)) > 1) {
            # print(sum(split(x, my.group), na.rm = TRUE))

            if (shapiro.test(x) >= 0.05) {
                wilcox.test(x ~ my.group)
            } else if(var.test(x ~ my.group)$p.value >= 0.05) {
                t.test(x ~ my.group)
            } else {
                t.test(x ~ my.group, var.equal=TRUE)
            }
        } else {
            "Insufficient sample size for inference testing"
        })

    # np1 <- lapply(test, function(x) wilcox.test(x ~ my.group))

    ds3 <- lapply(seq_along(ds1), function(i) list(results = ds2[[i]],
                                                   normality = sw1[[i]],
                                                   comparison = tt3[[i]]))

    names(ds3) <- names(ds1)

    cont.data <- ds3

    # analyze categorical data
    test <- my.data[, !c(cont)]

    ds1 <- lapply(test, function(x) table(x, my.group))
    ds2 <- lapply(test, function(x) Summarize(my.group ~ x, percent = "column",
                                              addtotal = FALSE))
    csq <- lapply(test, function(x)
        if (sum(!duplicated(x)) == 1) {
            "Must be values in at least two groups to perform chi-squared"
        } else if (is.factor(x)) {
            chisq.test(x, my.group)
        } else if (is.logical(x) & sum(x, na.rm = TRUE) > 0) {
            chisq.test(x, my.group)
        } else {
            "Chi-squared could not be performed"
        })

    ds3 <- lapply(seq_along(ds1), function(i) list(counts = ds1[[i]],
                                                   percents = ds2[[i]],
                                                   chi.sq = csq[[i]]))
    names(ds3) <- names(ds1)

    cat.data <- ds3

    return(c(cont.data, cat.data))
}
