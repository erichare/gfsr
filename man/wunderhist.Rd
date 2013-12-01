\name{wunderhist}
\alias{wunderhist}
\title{Get the weather history for a given date and station from Weather Underground}
\usage{
  wunderhist(station, validdate)
}
\arguments{
  \item{station}{A station code as a character vector
  representing any of the valid stations for which
  forecasts are issued}

  \item{validdate}{The date of the forecast as a character
  string or date object}
}
\value{
  data frame containing the weather history for a given
  station and date
}
\description{
  Get the weather history for a given date and station from
  Weather Underground
}
\examples{
wunderhist("KSEA", "2013-11-23")
}

