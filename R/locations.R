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
#' @example
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


#' Build Location Template
#' 
#' Create a \code{tibble} representing the breeDBase upload 
#' template for the provided locations
#' 
#' @param locations Vector of Locations to add to the template
#' 
#' @return A \code{tibble} representation of the upload template
#' 
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
        "Altitude" = numeric()
    )

    # Return blank template if no accessions provided
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
            "Altitude" = location@altitude
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