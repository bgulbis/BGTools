# reports.R

#' Create a results document
#'
#' \code{result_docx} make a docx object to insert results into
#'
#' This function creates a docx object and inserts project title, authors, and
#' report date into the object. The docx object can then be written to a
#' Microsoft Word document.
#'
#' @param project A character string with the name of the project
#' @param authors A character string with the name of the authors
#' @param template An optional character string with the filename of the docx
#'   file to be used as a template
#' @param title.names An optional character vector with names of title styles
#'   used
#' @param title.style An optional character string with the name of the title
#'   style to use for project title, defaults to TitleDoc
#' @param bookmark An optional character string with the name of the bookmark in
#'   the Word document template where the report should start, defaults to
#'   "start"
#'
#' @return A docx object
#'
#' @seealso \code{\link[ReporteRs]{docx}}
#'
#' @export
result_docx <- function(project, authors, template = NULL, title.names = NULL,
                        title.style = "TitleDoc", bookmark = "start") {
    # if no template specified, use default
    if (is.null(template)) {
        template <- system.file("templates", "results_template.docx",
                                package = "BGTools")
    }

    # make docx object
    mydoc <- ReporteRs::docx(template = template)

    # declare title styles
    if (is.null(title.names)) {
        title.names <- c("TitleDoc", "SubtitleCentered", "rTableLegend")
    }
    mydoc <- ReporteRs::declareTitlesStyles(mydoc, stylenames = title.names)

    mydoc <- ReporteRs::addParagraph(mydoc, project, stylename = title.style,
                                     bookmark = bookmark)
    mydoc <- ReporteRs::addTitle(mydoc, authors, level = 2)

    # add current date to report
    date <- format(Sys.Date(), "%B %d, %Y")
    mydoc <- ReporteRs::addTitle(mydoc, date, level = 2)

    return(mydoc)
}

#' Write docx to Word
#'
#' \code{write_docx} save a docx object to a Microsoft Word document
#'
#' This function takes a docx object and optionally inserts a citation for R,
#' then writes the docx object to a Microsoft Word document.
#'
#' @param mydoc A docx object
#' @param file.name A character string with the name of the project
#' @param add.citation An optional logical, if TRUE (default) a citation for R
#'   is added to the document
#' @param prep An optional character string with the name of the person who
#'   prepared the data analysis, ignored if add_citation is FALSE
#'
#' @return A docx object
#'
#' @seealso \code{\link[ReporteRs]{writeDoc}}
#'
#' @export
write_docx <- function(mydoc, file.name, add.citation = TRUE, prep = NULL) {
    if (add.citation == TRUE) {
        if (is.null(prep)) {
            mydoc <- add_rcitation(mydoc)
        } else {
            mydoc <- add_rcitation(mydoc, prep)
        }
    }

    ReporteRs::writeDoc(mydoc, file = file.name)

}

#' Create a results table using tableone
#'
#' \code{create_tableone} make a table with results to be inserted into
#' Microsoft Word
#'
#' This function takes a data frame and converts it to a TableOne object.
#'
#' @param test A data frame with the data to be summarized
#' @param group An optional character character vector indicating grouping
#'   column(s); default is no groups
#' @param ident An optional character string with the name of the subject
#'   identifier column
#'
#' @return A TableOne object
#'
#' @seealso \code{\link[tableone]{CreateTableOne}}
#'
#' @export
create_tableone <- function(test, group = NULL, ident = "pie.id") {
    # get list of variables to be reported; remove patient identifier column
    vars <- names(test)
    vars <- vars[vars != ident]

    # find categorical variables by removing all numeric variables
    cat <- purrr::discard(test, is.numeric)
    catVars <- names(cat)

    # create tableone, use print to make a data frame
    if (is.null(group)) {
        tab <- tableone::CreateTableOne(vars = vars, data = test,
                                        factorVars = catVars)
    } else {
        # remove grouping column
        vars <- vars[vars != group]
        tab <- tableone::CreateTableOne(vars, strata = group, data = test,
                                        factorVars = catVars)
    }
    return(tab)
}

#' Create a results table
#'
#' \code{result_table} make a table with results to be inserted into Microsoft
#' Word
#'
#' This function takes a data frame and converts it to a FlexTable. If mydoc is
#' a docx object, then the FlexTable is added to the docx object which is
#' returned. The docx object can then be written to a Microsoft Word document.
#'
#' If mydoc is the string "html", then an HTML FlexTable is returned. If mydoc
#' is "flextable", then the FlexTable object is returned. If mydoc is "cat",
#' then the detailed tableone summary of the categorical variables is returned.
#' If mydoc is "cont", then the detailed tableone summary of the continuous
#' variables is returned.
#'
#' @param mydoc Either a docx object, or string as "html", "flextable", "cat",
#'   "cont", or "table"
#' @param test A data frame
#' @param table.title An optional character string
#' @param group An optional character string indicating the name of the column
#'   to group on; defaults to "group", set group to NULL to remove grouping
#' @param cram An optional character vector of column names or a logical; if
#'   \code{TRUE} then all logical and 2-level factor variables will report both
#'   levels
#'
#' @return A docx object
#'
#' @seealso \code{\link[ReporteRs]{FlexTable}}
#'
#' @export
result_table <- function(mydoc, test, table.title = "", group = "group",
                         cram = NULL) {
    # determine which variables are continous
    cont <- purrr::keep(test, is.numeric)
    cont.vars <- names(cont)

    cram.vars <- ""

    # determine which variables are logical or factors with only two levels
    if (!is.null(cram)) {
        if (is.character(cram)) {
            cram.vars <- cram
        } else if (cram == TRUE) {
            cram.vars <- purrr::keep(test, is.logical)
            cram.vars <- names(cram.vars)

            cram.factor <- purrr::keep(test, is.factor)
            cram.factor <- purrr::keep(cram.factor, ~ length(levels(.x)) == 2)
            cram.vars <- c(cram.vars, names(cram.factor))
        }
    }

    # not.nrml.vars <- ""
    #
    # if (normal == "auto") {
    #     # if there are continuous variables, perform normality testing
    #     if (length(cont) > 0) {
    #         nrml <- normal_test(cont)
    #
    #         not.nrml <- dplyr::filter_(nrml, .dots = list(~p.value < 0.05))
    #         not.nrml.vars <- not.nrml$data.name
    #     }
    # } else if (normal == "medians") {
    #     not.nrml.vars <- cont.vars
    # }

    # create tableone, use print to make a data frame
    if (is.null(group)) {
        tab <- create_tableone(test)
    } else {
        tab <- create_tableone(test, group)
    }

    tab.cat <- print(tab$CatTable, printToggle = FALSE, cramVars = cram.vars)
    tab.mean <- print(tab$ContTable, printToggle = FALSE)
    tab.median <- print(tab$ContTable, printToggle = FALSE, nonnormal = cont.vars)
    tab.join <- rbind(tab.cat, tab.mean, tab.median)

    # get the FlexTable object
    mytable <- make_flextable(tab.join)

    # if mydoc is docx object, insert FlexTable into docx object; if a string
    # "html" then return the table as HTML, otherwise return the FlexTable
    # object
    if (class(mydoc) == "docx") {
        mydoc <- ReporteRs::addParagraph(mydoc, "")

        # add title before table, will output as "Table X: Title"
        mydoc <- ReporteRs::addTitle(mydoc, table.title, level = 3)

        # add the FlexTable to docx object and return
        mydoc <- ReporteRs::addFlexTable(mydoc, mytable)
    } else if (mydoc == "html") {
        mydoc <- ReporteRs::as.html(mytable)
    } else if (mydoc == "flextable") {
        return(mytable)
    } else if (mydoc == "cont") {
        return(summary(tab$ContTable))
    } else if (mydoc == "cat") {
        return(summary(tab$CatTable))
    } else {
        return(tab.join)
    }

    mydoc
}

# make a FlexTable
make_flextable <- function(df, zebra = TRUE) {
    # set table header properties
    hdr.cell <- ReporteRs::cellProperties(background.color = "#003366")
    hdr.txt <- ReporteRs::textBold(color = "white", font.family = "Calibri")

    # convert data frame into FlexTable object, format table
    mytable <- ReporteRs::FlexTable(df, add.rownames = TRUE,
                                    header.cell.props = hdr.cell,
                                    header.text.props = hdr.txt)
    mytable[] <- ReporteRs::textProperties(font.family = "Calibri",
                                           font.size = 10)

    if (zebra == TRUE) {
        mytable <- ReporteRs::setZebraStyle(mytable, odd = "#eeeeee",
                                            even = "white")
    }

    return (mytable)
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

#' Create multiple result tables separated by a factor
#'
#' \code{result_table2} make multiple tables with results to be inserted into
#' Microsoft Word
#'
#' This function takes a data frame and converts it into a separate FlexTable
#' for each factor of a variable. The FlexTables are then added to the docx
#' object which is returned. The docx object can then be written to a Microsoft
#' Word document. Similar to \code{\link{result_table}} except the data is first
#' separated into multiple data frames based on the split.by column.
#'
#' @param mydoc A docx object
#' @param test A data frame
#' @param split.by A character string indicating the name of the column used to
#'   separate the data into multiple data frames
#' @param table.title A character string, will proceed the name of the factor
#' @param group An optional character string indicating the name of the column
#'   to group on; defaults to "group", set group to NULL to remove grouping
#'
#' @return A docx object
#'
#' @seealso \code{\link[ReporteRs]{FlexTable}}
#'
#' @export
result_table2 <- function(mydoc, test, split.by, table.title, group = "group") {
    # make a character vector of the values which will be used to split the
    # table
    split.col <- as.name(split.by)
    dots <- list(lazyeval::interp(quote(x), x = split.col))
    splits <- dplyr::select_(test, .dots = dots)
    splits <- dplyr::distinct_(splits, .keep_all = TRUE)
    splits <- as.character(purrr::as_vector(splits))

    # for each value, make a separate result_table
    for (i in 1:length(splits)) {
        # filter only to the current value
        dots <- list(lazyeval::interp(~x == splits[[i]], x = split.col))
        tbl.test <- dplyr::filter_(test, .dots = dots)
        # remove the split.by column so it's not used in analysis
        dots <- list(lazyeval::interp(quote(-x), x = split.col))
        tbl.test <- dplyr::select_(tbl.test, .dots = dots)
        # create a new title for each table, prefix with table.title value
        new.title <- paste0(table.title, ": ", splits[[i]])
        # make new result_table and store in docx object
        mydoc <- result_table(mydoc, tbl.test, new.title, group)
    }

    return(mydoc)
}

#' Create a result table for regression model
#'
#' \code{result_regrmod} make a table with results of a regression model to be
#' inserted into Microsoft Word
#'
#' This function takes a regression model and saves the results in a FlexTable,
#' which is then added to the docx object which is returned. The docx object can
#' then be written to a Microsoft Word document.
#'
#' @param mydoc A docx object
#' @param mod A regression model
#' @param table.title A character string
#' @param exp An optional logical, passed to ShowRegTable
#'
#' @return A docx object
#'
#' @seealso \code{\link[tableone]{ShowRegTable}}
#'
#' @export
result_regrmod <- function(mydoc, mod, table.title, exp = TRUE) {
    # make a matrix with regression model results
    tab <- tableone::ShowRegTable(mod, exp = exp, printToggle = FALSE)

    # get the FlexTable object
    mytable <- make_flextable(tab)

    mydoc <- newline(mydoc)

    # add title before table, will output as "Table X: Title"
    mydoc <- ReporteRs::addTitle(mydoc, table.title, level = 3)

    # add the FlexTable to docx object and return
    mydoc <- ReporteRs::addFlexTable(mydoc, mytable)

    mydoc <- newline(mydoc)

    # add model statistics
    stats.mod <- t(broom::glance(mod))
    colnames(stats.mod) <- "model statistics"
    mytable <- make_flextable(stats.mod)
    mydoc <- ReporteRs::addFlexTable(mydoc, mytable)

    return(mydoc)
}

#' Add a ggplot2 graph to docx object
#'
#' \code{result_plot} add a ggplot2 graph to the docx object which can then be
#' inserted into Microsoft Word
#'
#' This function takes a ggplot2 graph and adds the plot to the docx object
#' which is returned. The docx object can then be written to a Microsoft Word
#' document.
#'
#' @param mydoc A docx object
#' @param graph A ggplot2 object
#' @param title An optional character string, if not NULL (default), the title
#'   will be added above the plot
#'
#' @return A docx object
#'
#' @seealso \code{\link[ReporteRs]{addPlot}}
#'
#' @export
result_plot <- function(mydoc, graph, title = NULL) {
    mydoc <- newline(mydoc)

    # if title given, add it before the plot
    if (!is.null(title)) {
        mydoc <- ReporteRs::addParagraph(mydoc, title,
                                         stylename = "SectionTitle")
        mydoc <- newline(mydoc)
    }

    # add the plot
    mydoc <- ReporteRs::addPlot(mydoc, fun = print, x = graph)

    return(mydoc)
}

#' Add citation for data preparation
#'
#' \code{add_rcitation} add a citation for R to a Microsoft Word document
#'
#' This function takes a docx object and adds paragraphs which cite R and the
#' name of the person who prepared the data analysis.
#'
#' @param mydoc A docx object
#' @param prep An optional character string with the name of the person who
#'   prepared the data analysis
#'
#' @return A docx object
#'
#' @export
add_rcitation <- function(mydoc, prep = "Brian Gulbis") {
    ref <- ReporteRs::pot("Data processed using ") + R.version.string +
        " on a " + .Platform$OS.type + " " + .Platform$r_arch + " system."
    prepby <- paste("Prepared by:", prep)
    citeTxt <- ReporteRs::pot(citation()$textVersion)

    newpar <- ReporteRs::addParagraph
    # newline <- ReporteRs::addParagraph(mydoc, "")

    mydoc <- newline(mydoc)
    mydoc <- newpar(mydoc, "Citation", stylename = "SectionTitle")
    mydoc <- newpar(mydoc, prepby)
    mydoc <- newline(mydoc)
    mydoc <- newpar(mydoc, ref)
    mydoc <- newline(mydoc)
    mydoc <- newpar(mydoc, "To cite R in publications, use:")
    mydoc <- newpar(mydoc, citeTxt)

    return(mydoc)
}

# insert a new line
newline <- function(mydoc) {
    mydoc <- ReporteRs::addParagraph(mydoc, "")
    return(mydoc)
}
