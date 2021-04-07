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

# List of configurable stock properties for Accessions
options(breedbase.standard_stock_props = c("population_name", "organization_name", "synonym", "PUI"))
options(breedbase.editable_stock_props = c("variety", "released_variety_name", "donor", "donor institute", "donor PUI", "country of origin", "state", "institute code", "institute name", "biological status of accession code", "notes", "accession number", "seed source", "type of germplasm storage code", "acquisition date", "location_code", "ploidy_level", "genome_structure", "ncbi_taxonomy_id", "transgenic", "introgression_parent", "introgression_backcross_parent", "introgression_map_version", "introgression_chromosome", "introgression_start_position_bp", "introgression_end_position_bp", "purdy_pedigree", "filial_generation"))