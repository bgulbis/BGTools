% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reports.R
\name{result_table}
\alias{result_table}
\title{Create a results table}
\usage{
result_table(mydoc, test, table.title = "", group = "group", cram = NULL)
}
\arguments{
\item{mydoc}{Either a docx object, or string as "html", "flextable", "cat",
"cont", or "table"}

\item{test}{A data frame}

\item{table.title}{An optional character string}

\item{group}{An optional character string indicating the name of the column
to group on; defaults to "group", set group to NULL to remove grouping}

\item{cram}{An optional character vector of column names or a logical; if
\code{TRUE} then all logical and 2-level factor variables will report both
levels}
}
\value{
A docx object
}
\description{
\code{result_table} make a table with results to be inserted into Microsoft
Word
}
\details{
This function takes a data frame and converts it to a FlexTable. If mydoc is
a docx object, then the FlexTable is added to the docx object which is
returned. The docx object can then be written to a Microsoft Word document.

If mydoc is the string "html", then an HTML FlexTable is returned. If mydoc
is "flextable", then the FlexTable object is returned. If mydoc is "cat",
then the detailed tableone summary of the categorical variables is returned.
If mydoc is "cont", then the detailed tableone summary of the continuous
variables is returned.
}
\seealso{
\code{\link[ReporteRs]{FlexTable}}
}

