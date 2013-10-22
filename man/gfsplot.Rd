\name{gfsplot}
\alias{gfsplot}
\title{Get a plot of a variable(s) over time for a station(s)}
\usage{
  gfsplot(station, measure)
}
\arguments{
  \item{station}{A station code as a character vector
  representing any of the valid stations for which
  forecasts are issued}

  \item{measure}{A variable from the forecast table to plot
  over time}
}
\value{
  plot of the measure variable over time for the station
}
\description{
  Get a plot of a variable(s) over time for a station(s)
}
\examples{
gfsplot("KSEA", "HLT")
gfsplot(c("KSEA", "KDSM"), "DPT")
}

