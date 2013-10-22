#' Get the current forecast table from the GFS
#' 
#' @param station A station code as a character vector representing any of the valid stations for which forecasts are issued
#' 
#' @return data frame containing the forecast up to hour 192 for the given station
#' 
#' @export
#' 
#' @examples
#' gfs("KSEA")
#' gfs("KDSM")
#' 
gfs <- function(station) {
    ## Grab the data
    webpage <- sapply(station, function(x) { getURL(paste("http://www.nws.noaa.gov/cgi-bin/mos/getmex.pl?sta=", x, sep = ""))})
    webpage <- readLines(tc <- textConnection(webpage)); close(tc)
    
    ## Choose only the relevant lines
    date.text <- webpage[grep(" UTC ", webpage)]
    forecast.text.tmp <- webpage[grep(" FHR | X/N | N/X | TMP | DPT | CLD | WND | P12 | Q12 | T12 ", webpage)]
    forecast.text <- gsub(" N/X | X/N ", " HLT ", forecast.text.tmp)
    
    ## Parse the date
    fixed.date <- strsplit(date.text, split = " ")[[1]]
    fixed.date <- fixed.date[-which(fixed.date == "")]
    forecast.date.temp <- paste(fixed.date[5:6], collapse = " ")
    forecast.date <- as.POSIXct(strptime(forecast.date.temp, format = "%m/%d/%Y %H%M"), tz = "GMT")
    
    ## Strip out unwanted characters
    fixed <- strsplit(forecast.text, split = " |\\|")
    fixed.list <- lapply(1:length(forecast.text), function(x){fixed[[x]][-which(fixed[[x]] %in% c(""))]})
    final.list.tmp <- lapply(fixed.list, function(col){col[-1]})
    
    ## Truncate to 15 (24 - 192)
    final.list <- lapply(final.list.tmp, function(col){
        if (length(col) > 15) col[1:15]
        else if (length(col) < 15) c(col, rep(NA, 15 - length(col)))
        else col
    })
    
    table.names <- unlist(lapply(fixed.list, function(col){col[1]}))[1:9]
    forecast.table <- NULL
    for (i in 1:length(station)) {
        df <- data.frame(final.list[(1 + (9 * (i - 1))):(9 *i)])
        names(df) <- table.names
        forecast.table <- rbind(forecast.table, df)
    }
    
    ## Convert to a data frame
    forecast.table[,!(names(forecast.table) %in% c("FHR", "CLD"))] <- sapply(forecast.table[,!(names(forecast.table) %in% c("FHR", "CLD"))], function(x){as.numeric(as.character(x))})
    
    ## Add Date
    final.table <- cbind(DAT = forecast.date, STA = rep(station, each = 15), forecast.table)
    
    return(final.table)
}
