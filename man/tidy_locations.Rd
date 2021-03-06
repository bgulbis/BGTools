% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_data.R
\name{tidy_locations}
\alias{tidy_locations}
\title{Tidy locations}
\usage{
tidy_locations(raw.data)
}
\arguments{
\item{raw.data}{A data frame with location data}
}
\value{
A data frame
}
\description{
\code{tidy_locations} tidy hospital location data
}
\details{
This function takes a data frame with hospital location history and produces
a tidy version with unit arrival and departure data. It accounts for
incorrect departure time from raw EDW data by calculating the departure time
using the arrival time of the next unit (unless it was the patient's last
unit during the hospitalization). It also combines multiple rows of data when
the patient did not actually leave that unit. The data should be read in by
\code{\link[BGTools]{read_edw_data}}.
}

