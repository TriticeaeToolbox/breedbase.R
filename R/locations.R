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
#' The \code{\link{lookupNOAAStationId}} function can be used to obtain the NOAA
#' Station ID of the location (using its lat and lon) if it is unknown.
#' 
#' Use the \code{\link{getCountryCodes}} function to get a list of supported country 
#' codes and the \code{\link{getLocationTypes}} function to get a list of supported 
#' location types.
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
#' @param noaa_station_id (optional) NOAA Station ID (ex: GHCND:US1NYTM0042)
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
#' #noaa_station_id <- lookupNOAAStationID(geo$latitude, geo$longitude)
#' noaa_station_id <- "none"
#' location <- Location(
#'      "Caldwell - Ithaca, NY", 
#'      "ITH_CALD",
#'      "USA",
#'      "United States of America",
#'      "Cornell",
#'      "Field",
#'      geo$latitude,
#'      geo$longitude,
#'      geo$altitude,
#'      noaa_station_id
#' )
#' 
#' @return Location
#' 
#' @family Location
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
    altitude = NULL,
    noaa_station_id = NULL
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

    # Get NOAA station id, if missing
    if ( is.null(noaa_station_id) ) {
        noaa_station_id <- lookupNOAAStationID(latitude, longitude)
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
        altitude = altitude,
        noaa_station_id = noaa_station_id
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
#' @examples
#' caldwell <- geocodeLocation("2 Caldwell Dr, Ithaca, NY")
#' lat <- caldwell$latitude
#' lon <- caldwell$longitude
#' alt <- caldwell$altitude
#' 
#' @return A list with components:
#' \describe{
#'   \item{latitude}{Location latitude (decimal degrees)}
#'   \item{longitude}{Location longitude (decimal degrees)}
#'   \item{altitude}{Location elevation (meters)}
#' }
#' 
#' @family Location
#' @import utils httr rjson
#' @export
geocodeLocation <- function(location) {

    # Make OSM API Request
    params = list(q = location, format = "json")
    body <- api(OSM_API_SEARCH, params)

    # Location could NOT be geocoded!
    if ( is.null(body) || length(body) == 0 ) {
        print(sprintf("ERROR: Could not geocode location [%s]!", location))
        return(list(
            latitude = 0,
            longitude = 0,
            altitude = 0
        ))
    }

    # Get first location
    location = body[[1]]

    # Format return
    rtn <- list(
        latitude = as.double(location$lat),
        longitude = as.double(location$lon)
    )

    # Make GMRT API Request
    params <- list(latitude = location$lat, longitude = location$lon, format = "json")
    body <- api(GMRT_API_PS, params)

    # Add elevation to return
    if ( !is.null(body) && exists("elevation", body) ) {
        rtn$altitude <- as.double(body$elevation)
    }

    # Return the list
    return(rtn)

}


#' Lookup NOAA Station ID
#' 
#' Lookup the NOAA Station ID for the specified location.  The 
#' Location must be specified by its latitude and longitude 
#' (as decimal degrees) - if these are unknown the \code{\link{geocodeLocation}} 
#' function can be used to get the coordinates of a location by 
#' address, town, etc.
#' 
#' The query will search for NOAA stations within the specified max radius 
#' centered around the location position.  If multiple stations are found,
#' the ID of the first station is returned.
#'
#' @param lat Location latitiude (decimal degrees)
#' @param lon Location longitude (decimal degrees)
#' @param radius Max Search radius (miles, default=25)
#' 
#' @return The NOAA Station ID
#' 
#' @family Location
#' @import httr rjson
#' @export
lookupNOAAStationID <- function(lat, lon, max_radius=25) {
    station = NULL
    radius = 1
    while ( radius <= max_radius ) {
        station = queryNOAAStations(lat, lon, radius)
        if ( !is.null(station) ) {
            return(station$id)
        }
        else {
            radius = ifelse(radius == 1, 5, radius+5)
        }
    }
    return("none")
}


#' Build Location Template
#' 
#' Create a \code{tibble} representing the breeDBase upload 
#' template for the provided locations
#' 
#' @param locations Vector of Locations to add to the template
#' 
#' @return A \code{tibble} representation of the upload template
#' 
#' @family Location
#' @import dplyr tibble
#' @export
buildLocationTemplate <- function(
    locations = NULL
) {

    # Set template headers
    template <- tibble::tibble(
        "Name" = character(),
        "Abbreviation" = character(),
        "Country Code" = character(),
        "Country Name" = character(),
        "Program" = character(),
        "Type" = character(),
        "Latitude" = numeric(),
        "Longitude" = numeric(),
        "Altitude" = numeric(),
        "NOAA Station ID" = character()
    )

    # Return blank template if no locations provided
    if ( is.null(locations) ) {
        return(template)
    }

    # Ensure a vector
    locations <- c(locations)

    # Parse each location
    for ( location in locations ) {
        row <- tibble::tibble(
            "Name"  = location@name,
            "Abbreviation" = location@abbreviation,
            "Country Code" = location@country_code,
            "Country Name" = location@country_name,
            "Program" = paste(location@program, sep="&"),
            "Type" = location@type,
            "Latitude" = location@latitude,
            "Longitude" = location@longitude,
            "Altitude" = location@altitude,
            "NOAA Station ID" = location@noaa_station_id
        )
        template <- dplyr::bind_rows(template, row)
    }

    # Clean template
    for ( name in names(template) ) {
        template[name][which(template[name] == ""),] <- NA
    }

    # Return the template
    return(template)

}


#' Write Location Template
#' 
#' Write a breeDBase upload template (.xls file) for locations
#' 
#' @param input Either a vector of Locations to include in the template OR 
#' a \code{tibble} representation of the upload template
#' @param output The file path to the output .xls file
#' @param chunk Chunk the file into parts with up to `chunk` number of lines per file
#' 
#' @family Location
#' @import WriteXLS
#' @export
writeLocationTemplate <- function(
    input = NULL,
    output = NULL,
    chunk = NULL
) {

    # Check for required arguments
    if ( is.null(input) ) {
        stop("Cannot write Location Template file: input of a template as a tibble or vector of locations is required")
    }
    if ( is.null(output) ) {
        stop("Cannot write Location Template file: output of the file path to the .xls file is required")
    }

    # Create template if not provided one
    if ( !("tbl_df" %in% is(input)) ) {
        input <- buildLocationTemplate(input)
    }

    # Set output extension
    if ( !grepl("\\.xls$", output) ) {
        output <- paste(output, ".xls", sep="")
    }

    # Split the input file, if chunk is provided
    if ( !is.null(chunk) ) {
        max <- nrow(input)
        index <- 1
        start <- 1
        end <- ifelse(max < chunk, max, chunk)
        while ( end <= max ) {
            
            # Subset data and write the subset
            subset <- input[c(start:end),]
            subset_output <- gsub("\\.xls$", paste0("_part", index, ".xls"), output)
            writeLocationTemplate(subset, subset_output)

            if ( end == max ) {
                end <- max + 1
            }
            else {
                index <- index + 1
                start <- end + 1
                end <- end + chunk
                end <- ifelse(end > max, max, end)
            }
        }
    }

    # Write the entire file
    else {
        print(sprintf("Writing Location Template: %s", output))
        WriteXLS::WriteXLS(input, output)
    }

}







## ======== PRIVATE HELPER FUNCTIONS ======== ##


# Query the NOAA Stations API for GHCND Stations located within the 
# specified radius of the search location.  Return the full Station
# object for the first GHCND station returned by the API.  If no 
# matching Station is found, NULL is returned.
queryNOAAStations <- function(lat, lon, radius) {

    # Calculate bounding box coordinates with geodetic approximation (WGS84)
    a <- 6378137            # Radius of earth at equator (m)
    e2 <- 0.00669437999014  # eccentricity squared
    m <- 1609.344           # mile to meters converstion factor
    r <- pi / 180           # degrees to radians conversion factor

    # Distance of latitude in miles
    d1 <- r*a*(1-e2) / (1-e2*sin(lat*r)^2)^(3/2) / m

    # Distane of longitude in miles
    d2 <- r*a*cos(lat*r) / sqrt(1-e2*sin(lat*r)^2) / m

    # Bounding Box Coords
    minlat = lat - radius / d1
    maxlat = lat + radius / d1
    minlon = lon - radius / d2
    maxlon = lon + radius / d2

    # Set extent
    extent = paste(minlat, minlon, maxlat, maxlon, sep=",")

    # Make API Request
    params <- list(extent = extent)
    body <- api(NOAA_STATIONS, params, NOAA_TOKEN)

    # Parse Stations
    stations <- body$results
    if ( !is.na(stations) && !is.null(stations) ) {
        for ( station in stations ) {
            if ( grepl("^GHCND", station$id) ) {
                return(station)
            }
        }
    }

    return(NULL)
}


# Make an API request to the specified URL
# url = request URL
# params = (optional) query parameters, as a list
# token = (optional) token header value
# Return the parsed JSON body
api <- function(url, params=NULL, token=NULL) {
    body <- tryCatch({
        ua <- paste0("breedbase.R/", utils::packageVersion("breedbase"), " (httr/", utils::packageVersion("httr"), ")")
        resp <- httr::GET(
            url, 
            add_headers("Content-Type" = "application/json", "token" = token, "User-Agent" = ua), 
            query=params
        )
        body <- httr::content(resp, "text", encoding="UTF-8")
        body <- rjson::fromJSON(body)
        return(body)
    }, error = function(e) {
        stop(sprintf("Could not make API request [%s]", url))
    })
}


# OSM Search API
OSM_API_SEARCH <- "https://nominatim.openstreetmap.org/search"

# GMRT Elevation PointServer API
GMRT_API_PS <- "https://www.gmrt.org/services/PointServer"

# NOAA Web Services Token
NOAA_TOKEN <- "UbRtMRShXhSSqHLWfpUpbeOoPiksgpLM"

# NOAA Stations API Endpoint
NOAA_STATIONS <- "https://www.ncdc.noaa.gov/cdo-web/api/v2/stations"