#' Accession Class
#' 
#' An S4 Class to represent a breeDBase accession
#' 
#' The `accession_name` and `species_name` are required, all other fields are optional.
#' 
#' @slot accession_name Accession Name (must be unique)
#' @slot species_name Species Name (must exist in the database)
#' @slot properties (optional) a list of additional accession stock properties, where the list key is a stock property name supported by the breedbase server and the list value is either a string value or a vector of string values.
#' 
#' @family Accession
#' @importFrom methods is new slot<-
#' @export
setClass(
    "Accession",

    slots = list(
        accession_name = "character",
        species_name = "character",
        properties = "list"
    ),

    prototype = list(
        accession_name = NA_character_,
        species_name = NA_character_,
        properties = list()
    ),
    
    validity = function(object) {
        if ( is.na(object@accession_name) ) {
            return("Accession Name is required")
        }
        if ( is.na(object@species_name) ) {
            return("Species Name is required")
        }

        supported_props = getSupportedAccessionProperties()
        provided_props = names(object@properties)
        for ( prop in provided_props ) {
            if ( !(prop %in% supported_props) ) {
                return(sprintf("Accession property [%s] is not supported", prop))
            }
        }

        return(TRUE)
    }
)
