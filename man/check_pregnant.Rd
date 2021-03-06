% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_pregnant.R
\name{check_pregnant}
\alias{check_pregnant}
\title{Check for pregnant patients}
\usage{
check_pregnant(labs, icd9 = NULL, icd10 = NULL)
}
\arguments{
\item{labs}{A data frame with pregnancy test lab values}

\item{icd9}{An optional data frame with ICD-9 diagnosis codes; will not check
for the presence of ICD-9 codes indicating pregnancy if NULL (default)}

\item{icd10}{An optional data frame with ICD-10 diagnosis codes; will not
check for the presence of ICD-10 codes indicating pregnancy if NULL
(default)}
}
\value{
A data frame
}
\description{
\code{check_pregnant} takes data frames with diagnosis codes and labs and
returns a data frame with identifiers of pregnant patients
}
\details{
This function takes data frames with ICD9/10 diagnosis codes and a data frame
with urine/serum pregnancy test results, and returns a data frame with a list
of all patients who are pregnant. This can be used to exclude patients from
research studies.
}

