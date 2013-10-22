#' Get a plot of a variable(s) over time for a station(s)
#' 
#' @param station A station code as a character vector representing any of the valid stations for which forecasts are issued
#' @param measure A variable from the forecast table to plot over time
#' 
#' @return plot of the measure variable over time for the station
#' 
#' @export
#' 
#' @examples
#' gfsplot("KSEA", "HLT")
#' gfsplot(c("KSEA", "KDSM"), "DPT")
#' 
gfsplot <- function(station, measure) {
    require(ggplot2)
    
    ## Get the forecast table
    forecast <- gfs(station)
    valid.dates <- forecast$DAT + hours(as.numeric(as.character(forecast$FHR)))
    forecast$VTM <- valid.dates
    
    ggplot(data = forecast, aes_string(x = "VTM", y = measure, color = "STA", group = "STA")) + 
        geom_point() + 
        geom_line()
}
