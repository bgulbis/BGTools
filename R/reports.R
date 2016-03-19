# reports.R

#' Create a results table
#'
#' \code{result_table} make a table with results to be inserted into Microsoft
#' Word
#'
#' This function takes a data frame and outputs the results of the analysis to a
#' FlexTable, for inclusion in a Word document.
#'
#' @param mydoc A docx object
#' @param test A data frame
#' @param table.title A character string
#'
#' @return A docx object
#'
#' @seealso
#' \code{\link[tableone]{CreateTableOne}}
#' \code{\link[ReporteRs]{FlexTable}}
#'
#' @export
result_table <- function(mydoc, test, table.title) {
    vars <- names(test)
    vars <- vars[!(vars %in% c("pie.id", "group"))]

    cont <- purrr::keep(test, is.numeric)
    contVars <- names(cont)

    cram <- purrr::keep(test, is.logical)
    cramVars <- names(cram)

    not.nrmlVars <- ""

    # if there are continuous variables, perform normality testing
    if (length(cont) > 0) {
        nrml <- normal_test(cont)

        not.nrml <- dplyr::filter_(nrml, .dots = list(~p.value < 0.05))
        not.nrmlVars <- not.nrml$data.name
    }

    cat <- purrr::discard(test, is.numeric)
    catVars <- names(cat)

    tab <- tableone::CreateTableOne(vars, strata = "group", data = test,
                                    factorVars = catVars)
    tab1 <- print(tab, printToggle = FALSE, nonnormal = not.nrmlVars,
                  cramVars = cramVars)

    hdr.cell <- ReporteRs::cellProperties(background.color = "#003366")
    hdr.txt <- ReporteRs::textBold(color = "white", font.family = "Calibri")
    mydoc <- ReporteRs::addParagraph(mydoc, "")
    mydoc <- ReporteRs::addTitle(mydoc, table.title, level = 3)
    mytable <- ReporteRs::FlexTable(tab1, add.rownames = TRUE,
                                    header.cell.props = hdr.cell,
                                    header.text.props = hdr.txt)
    mytable[] <- ReporteRs::textProperties(font.family = "Calibri",
                                           font.size = 10)
    mytable <- ReporteRs::setZebraStyle(mytable, odd = "#eeeeee",
                                        even = "white")

    mydoc <- ReporteRs::addFlexTable(mydoc, mytable)

    return(mydoc)
}


# check for normality
normal_test <- function(x) {
    # only check continous data fields for normality
    y <- purrr::keep(x, is.numeric)
    # if the test produces an error, a default result will be returned instead
    nrml <- purrr::map_df(y, purrr::possibly(shapiro.test,
                       otherwise = list(statistic = NA, p.value = NA,
                                        method = "Shapiro-Wilk normality test",
                                        data.name = ".x[[i]]")))
    # rename the data so we know what fields were tested
    dots <- list(~names(y))
    nrml <- dplyr::mutate_(nrml, .dots = setNames((dots), "data.name"))
    # reorder the columns
    nrml <- dplyr::select_(nrml, .dots = list("data.name", "method",
                                              "statistic", "p.value"))
    return(nrml)
}
