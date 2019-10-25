#' Create Location
#' 
#' Create a Location containing all of the provided properties. 
#' Latitude, Longitude, and Altitude are optional and if not provided 
#' will be queried from the DataScienceToolkit API using the Location name. 
#' 
#' The \code{\link{geocodeLocation}} function can be used to obtain lat/lon/alt 
#' properties from a more specific location name (such as a street address), the 
#' results of which can be used to create the Location.
#' 
#' @param name Unique location name (must not conflict with an existing location name)
#' @param abbreviation Location abbreviation
#' @param country_code ISO Alpha-3 country code
#' @param country_name Uppercase english short name from the ISO standard
#' @param program Name of one or more existing Breeding Programs
#' @param type Location type (Farm, Field, Greenhouse, Screenhouse, Lab, Storage, Other)
#' @param latitude (optional) Location latitude (decimal degrees)
#' @param longitude (optional) Location longitude (decimal degrees)
#' @param altitude (optional) Location elevation (meters)
#' 
#' @examples
#' # Use the Location name to query lat/lon/alt properties
#' location <- Location(
#'      "Batavia, NY",
#'      "BAT",
#'      "USA",
#'      "United States of America",
#'      "Cornell",
#'      "Field"
#' )
#' 
#' # Geocode an address to use as lat/lon/alt properties
#' geo <- geocodeLocation("2 Caldwell Drive, Ithaca, NY")
#' location <- Location(
#'      "Caldwell - Ithaca, NY", 
#'      "ITH_CALD",
#'      "USA",
#'      "United States of America",
#'      "Cornell",
#'      "Field",
#'      geo$latitude,
#'      geo$longitude,
#'      geo$altitude
#' )
#' 
#' @return Accession
#' 
#' @export
Location <- function(
    name = NULL,
    abbreviation = NULL,
    country_code = NULL,
    country_name = NULL,
    program = NULL,
    type = NULL,
    latitude = NULL,
    longitude = NULL,
    altitude = NULL
) {

    # Check for required properties
    if ( is.null(name) ) {
        stop("Cannot create Location: name is required")
    }
    if ( is.null(abbreviation) ) {
        stop("Cannot create Location: abbreviation is required")
    }
    if ( is.null(country_code) ) {
        stop("Cannot create Location: country code is required")
    }
    if ( is.null(country_name) ) {
        stop("Cannot create Location: country name is required")
    }
    if ( is.null(program) ) {
        stop("Cannot create Location: breeding program name is required")
    }
    if ( is.null(type) ) {
        stop("Cannot create Location: location type is required")
    }

    # Get geocode info, if missing any
    if ( is.null(latitude) | is.null(longitude) | is.null(altitude) ) {
        props <- geocodeLocation(name)
        if ( is.null(latitude) ) {
            latitude <- props$latitude
        }
        if ( is.null(longitude) ) {
            longitude <- props$longitude
        }
        if ( is.null(altitude) ) {
            altitude <- props$altitude
        }
    }

    # Create Location
    location <- new(
        "Location",
        name = name,
        abbreviation = abbreviation,
        country_code = country_code,
        country_name = country_name,
        program = program,
        type = type,
        latitude = latitude,
        longitude = longitude,
        altitude = altitude
    )

    # Return the Location
    return(location)

}


#' Geocode Location
#' 
#' Lookup the coordinates and elevation for the specified 
#' location.  The location text will be parsed by DataScienceToolkit's
#' `street2coordinates` API into coordinates and the elevation will be 
#' queried using the `coordinates2statistics` API.
#' 
#' @param location Location to parse (ie Ithaca, NY; Caldwell Dr, Ithaca, NY; etc...)
#' 
#' @return list(latitude, longitude, altitude)
#' 
#' @import utils httr rjson
#' @export
geocodeLocation <- function(location) {
    
    # Encode location for URL
    location <- utils::URLencode(location)

    # Make API Request
    url <- paste(DSTK_API_COORDS, location, sep="/")
    body <- api(url)

    # Format return
    values <- body[[names(body)[[1]]]]
    rtn <- list(
        latitude = values$latitude,
        longitude = values$longitude
    )

    # Encode coordinates for URL
    coords <- paste(rtn$latitude, rtn$longitude, sep=",")
    coords <- utils::URLencode(coords)

    # Make API Request
    url <- paste(DSTK_API_STATS, coords, sep="/")
    body <- api(url)

    # Add elevation to return
    statistics <- body[[1]]$statistics
    rtn$altitude <- statistics$elevation$value

    # Return the list
    return(rtn)

}








## ======== PRIVATE HELPER FUNCTIONS ======== ##


# Make an API request to the specified URL
# Return the parsed JSON body
api <- function(url) {
    resp <- httr::GET(url, add_headers("Content-Type" = "application/json"))
    body <- httr::content(resp, "text", encoding="UTF-8")
    body <- rjson::fromJSON(body)
    return(body)
}


# Data Science Toolkit API Root URL
DSTK_API <- "http://www.datasciencetoolkit.org"

# Data Science Toolkit API street2coordinates URL
DSTK_API_COORDS <- paste(DSTK_API, "street2coordinates", sep="/")

# Data Science Toolkit API coordinates2statistics URL
DSTK_API_STATS <- paste(DSTK_API, "coordinates2statistics", sep="/")