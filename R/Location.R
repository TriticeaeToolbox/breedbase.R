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

        if ( !(object@country_code %in% COUNTRY_CODES) ) {
            return("Country code is not recognized - must be an ISO Alpha-3 country code.")
        }
        if ( !(object@type %in% LOCATION_TYPES) ) {
            return(paste0(
                "Location type is not recognized. Supported Location types: ",
                paste(LOCATION_TYPES, collapse=", ")
            ))
        }

        return(TRUE)
    }

)


# List of ISO Alpha-3 country codes
COUNTRY_CODES <- c('AFG','ALA','ALB','DZA','ASM','AND','AGO','AIA','ATA','ATG','ARG','ARM','ABW','AUS','AUT','AZE','BHS','BHR','BGD','BRB','BLR','BEL','BLZ','BEN','BMU','BTN','BOL','BES','BIH','BWA','BVT','BRA','IOT','VGB','BRN','BGR','BFA','BDI','CPV','KHM','CMR','CAN','CYM','CAF','TCD','CHL','CHN','HKG','MAC','CXR','CCK','COL','COM','COG','COK','CRI','CIV','HRV','CUB','CUW','CYP','CZE','PRK','COD','DNK','DJI','DMA','DOM','ECU','EGY','SLV','GNQ','ERI','EST','SWZ','ETH','FLK','FRO','FJI','FIN','FRA','GUF','PYF','ATF','GAB','GMB','GEO','DEU','GHA','GIB','GRC','GRL','GRD','GLP','GUM','GTM','GGY','GIN','GNB','GUY','HTI','HMD','VAT','HND','HUN','ISL','IND','IDN','IRN','IRQ','IRL','IMN','ISR','ITA','JAM','JPN','JEY','JOR','KAZ','KEN','KIR','KWT','KGZ','LAO','LVA','LBN','LSO','LBR','LBY','LIE','LTU','LUX','MDG','MWI','MYS','MDV','MLI','MLT','MHL','MTQ','MRT','MUS','MYT','MEX','FSM','MCO','MNG','MNE','MSR','MAR','MOZ','MMR','NAM','NRU','NPL','NLD','NCL','NZL','NIC','NER','NGA','NIU','NFK','MKD','MNP','NOR','OMN','PAK','PLW','PAN','PNG','PRY','PER','PHL','PCN','POL','PRT','PRI','QAT','KOR','MDA','REU','ROU','RUS','RWA','BLM','SHN','KNA','LCA','MAF','SPM','VCT','WSM','SMR','STP','SAU','SEN','SRB','SYC','SLE','SGP','SXM','SVK','SVN','SLB','SOM','ZAF','SGS','SSD','ESP','LKA','PSE','SDN','SUR','SJM','SWE','CHE','SYR','TJK','THA','TLS','TGO','TKL','TON','TTO','TUN','TUR','TKM','TCA','TUV','UGA','UKR','ARE','GBR','TZA','UMI','USA','VIR','URY','UZB','VUT','VEN','VNM','WLF','ESH','YEM','ZMB','ZWE')

# List of supported location types
LOCATION_TYPES <- c('Farm', 'Field', 'Greenhouse', 'Screenhouse', 'Lab', 'Storage', 'Other')
