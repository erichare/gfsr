#' Get the weather history for a given date and station from Weather Underground
#' 
#' @param station A station code as a character vector representing any of the valid stations for which forecasts are issued
#' @param validdate The date of the forecast as a character string or date object
#' 
#' @return data frame containing the weather history for a given station and date
#' 
#' @export
#' 
#' @examples
#' wunderhist("KSEA", "2013-11-23")
#' 
wunderhist <- function(station, validdate) {
    forecast.month <- month(validdate)
    forecast.day <- day(validdate)
    
    history.table <- readHTMLTable(paste("http://www.wunderground.com/history/airport", station, "2013", forecast.month, forecast.day, "DailyHistory.html", sep = "/"))$historyTable
    
    interest <- history.table[grep("Max Temperature|Min Temperature|Dew Point|Precipitation|^Wind Speed", history.table[,1]), ]
    interest <- interest[complete.cases(interest),]
    
    names(interest) <- c("Metric", "Actual", "Average", "Record")
    
    interest$Actual <- gsub("[^0-9.]", "", interest$Actual)
    interest$Average <- gsub("[^0-9.]", "", interest$Average)
    interest$Record <- gsub("[[:space:]][^$]*", "", interest$Record)
    
    interest$Metric <- factor(interest$Metric)
    
    interest$Actual <- as.numeric(interest$Actual)
    interest$Average <- as.numeric(interest$Average)
    interest$Record <- as.numeric(interest$Record)
    
    rownames(interest) <- 1:nrow(interest)
        
    return(interest)
}