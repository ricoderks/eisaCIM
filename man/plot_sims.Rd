% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_sims.R
\name{plot_sims}
\alias{plot_sims}
\title{Show the SIM traces}
\usage{
plot_sims(
  sim_data,
  rt_line = NULL,
  title = NULL,
  subtitle = NULL,
  peak_data = NULL
)
}
\arguments{
\item{sim_data}{Data frame containing all the SIMs. Needs three columns: rt = retention time (x), intensity = intensity (y),
sim = factor with the SIM names (used for faceting)}

\item{rt_line}{A numerical value. If not NULL a dashed line is shown at this position.}

\item{title}{Title of the plot (character).}

\item{subtitle}{Subtitle of the plot (character).}

\item{peak_data}{data frame containing all peak data. Output from find_peaks().}
}
\value{
A ggplot2 showing all the SIM traces.
}
\description{
Plot all SIM trace, including optional line to show where the compound elutes.
}
\author{
Rico Derks
}
