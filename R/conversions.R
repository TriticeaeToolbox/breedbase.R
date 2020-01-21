#' Convert bu/ac to kg/Ha
#' 
#' Convert grain yield units from bu/ac to kg/Ha
#' 
#' @param x grain yield in bu/ac
#' @param crop crop type ("barley", "oat", "wheat")
#' 
#' @return grain yield in kg/Ha
#' 
#' @export
convert_buac_kgHa <- function(x, crop=NULL) {
    if (is.null(crop)) stop("please provide the crop to convert bushels per acre to kg/ha")
    cropWt = list(barley = 48, oat = 32, wheat = 60)
    y <- x * cropWt[[crop]] * 0.453592 * 2.47105 # c(kgPerLb = 0.453592, acPerHa = 0.404686)
    return(y)
}


#' Convert bu/ac to g/m2
#' 
#' Convert grain yield units from bu/ac to g/m2
#' 
#' @param x grain yield in bu/ac
#' @param crop crop type ("barley", "oat", "wheat")
#' 
#' @return grain yield in g/m2
#' 
#' @export
convert_buac_gm2 <- function(x, crop=NULL) {
    if (is.null(crop)) stop("please provide the crop to convert bushels per acre to g/m2")
    y <- convert_buac_kgHa(x, crop) * 0.1
    return(y)
}


#' Convert kg/Ha to g/m2
#' 
#' Convert grain yield units from kg/Ha to g/m2
#' 
#' @param x grain yield in kg/Ha
#' 
#' @return grain yield in g/m2
#' 
#' @export
convert_kgHa_gm2 <- function(x) {
    y <- x/10
    return(y)
}


#' Convert g/m2 to kg/Ha
#' 
#' Convert grain yield units from g/m2 to kg/Ha
#' 
#' @param x grain yield in g/m2
#' 
#' @return grain yield in kg/Ha
#' 
#' @export
convert_gm2_kgHa <- function(x) {
    y <- x*10
    return(y)
}


#' Convert lbs/bu to g/L
#' 
#' Convert test weight units from lbs/bu to g/L
#' 
#' @param x test weight in lbs/bu
#' 
#' @return test weight in g/L
#' 
#' @export
convert_lbsbu_gL <- function(x) {
    y <- x / 35.2391 / 2.2046 *1000 # c(LperBu = 35.2391, lbPerKg = 2.2046)
    return(y)
}


#' Convert kg/hl to g/l
#' 
#' Convert test weight units from kg/hl to g/L
#' 
#' @param x test weight in kg/hl
#' 
#' @return test weight in g/L
#' 
#' @export
convert_kghl_gL <- function(x) {
    y <- x * 10
    return(y)
}


#' Convert in to cm
#' 
#' Convert height / length units from in to cm
#' 
#' @param x height in inches
#' 
#' @return height in cm
#' 
#' @export
convert_in_cm <- function(x) {
    y <- x * 2.54
    return(y)
}


#' Convert x/sq ft to x/m2
#' 
#' Convert a rate per square foot to a 
#' rate per square meters
#' 
#' @param x rate per square foot
#' 
#' @return rate per square meter
#' 
#' @export
convert_sqft_m2 <- function(x) {
    y <- x * (1/0.09290304)
    return(y)
}


#' Convert scale
#' 
#' Convert a value from an ordinal scale in one range to a
#' value in a new range (0-->5, 1-->10)
#' 
#' @param x the original value
#' @param oldmin the old range minimum
#' @param oldmax the old range maximum
#' @param newmin the new range minimum
#' @param newmax the new range maximum
#' 
#' @return the value in the new range
#' 
#' @export
convert_scale <- function(x, oldmin, oldmax, newmin, newmax) {
    oldrange <- oldmax - oldmin
    newrange <- newmax - newmin
    y <- (((x - oldmin) * newrange) / oldrange) + newmin
    return(y)
}
