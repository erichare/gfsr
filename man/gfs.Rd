\name{gfs}
\alias{gfs}
\title{Get the current forecast table from the GFS}
\usage{
  gfs(station)
}
\arguments{
  \item{station}{A station code as a character vector
  representing any of the valid stations for which
  forecasts are issued}
}
\value{
  data frame containing the forecast up to hour 192 for the
  given station
}
\description{
  Get the current forecast table from the GFS
}
\examples{
gfs("KSEA")
gfs("KDSM")
}

