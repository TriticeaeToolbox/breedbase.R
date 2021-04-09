#' Location Class
#' 
#' An S4 Class to represent a breeDBase Location
#' 
#' @slot name Unique location name (must not conflict with an existing location name)
#' @slot abbreviation Location abbreviation
#' @slot country_code ISO Alpha-3 country code
#' @slot country_name Uppercase english short name from the ISO standard
#' @slot program Name of one or more existing Breeding Programs
#' @slot type Location type (Farm, Field, Greenhouse, Screenhouse, Lab, Storage, Other)
#' @slot latitude Location latitude (decimal degrees)
#' @slot longitude Location longitude (decimal degrees)
#' @slot altitude Location elevation (meters)
#' @slot noaa_station_id The NOAA Station ID that is nearest and most relevant to the location (ex: GHCND:US1NYTM0042)
#' 
#' @family Location
#' @importFrom methods is new slot<-
#' @export
setClass(
    "Location",

    slots = list(
        name = "character",
        abbreviation = "character",
        country_code = "character",
        country_name = "character",
        program = "character",
        type = "character",
        latitude = "numeric",
        longitude = "numeric",
        altitude = "numeric",
        noaa_station_id = "character"
    ),

    prototype = list(
        name = NA_character_,
        abbreviation = NA_character_,
        country_code = NA_character_,
        country_name = NA_character_,
        program = NA_character_,
        type = NA_character_,
        latitude = NA_real_,
        longitude = NA_real_,
        altitude = NA_real_,
        noaa_station_id = NA_character_
    ),

    validity = function(object) {
        if ( is.na(object@name) ) {
            return("Location name is required")
        }
        if ( is.na(object@abbreviation) ) {
            return("Location abbreviation is required")
        }
        if ( is.na(object@country_code) ) {
            return("Country code is required")
        }
        if ( is.na(object@country_name) ) {
            return("Country name is required")
        }
        if ( is.na(object@program) ) {
            return("Breeding program is required")
        }
        if ( is.na(object@type) ) {
            return("Location type is required")
        }
        if ( is.na(object@latitude) ) {
            return("Location latitude is required")
        }
        if ( is.na(object@longitude) ) {
            return("Location longitude is required")
        }
        if ( is.na(object@altitude) ) {
            return("Location altitude is required")
        }
        if ( is.na(object@noaa_station_id) ) {
            return("NOAA Station ID is required")
        }

        if ( !(object@country_code %in% getBBOption("country_codes")) ) {
            return("Country code is not recognized - must be an ISO Alpha-3 country code.")
        }
        if ( !(object@type %in% getBBOption("location_types")) ) {
            return(paste0(
                "Location type is not recognized. Supported Location types: ",
                paste(getBBOption("location_types"), collapse=", ")
            ))
        }

        return(TRUE)
    }

)
