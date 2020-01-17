#' Trial Class
#' 
#' An S4 Class to represent a breeDBase phenotyping trial
#' 
#' The `trial_name`, `breeding_program`, `location`, `year`, `design_type`, and 
#' `description` are all required, all other fields are optional.
#' 
#' @slot trial_name A name for the trial (must be unique across the entire database)
#' @slot breeding_program The name of the breeding program that managed the trial (must exist in database)
#' @slot location The name of the location where the trial was held (must exist in database)
#' @slot year The year the trial was held
#' @slot design_type The shorthand for the design type (CRD, RCBD, Alpha, Augmented, MAD, Westcott, Lattice)
#' @slot description Additional text with any other relevant information about the trial
#' @slot trial_type (optional) The name of the trial type (must exist in the database)
#' @slot plot_width (optional) The plot width in meters
#' @slot plot_length (optional) The plot length in meters
#' @slot field_size (optional) The field size in hectares
#' @slot planting_date (optional) The date of planting in YYYY-MM-DD format
#' @slot harvest_date (optional) The date of harvest in YYYY-MM-DD format
#' @slot plots (optional) A vector of Plots that are used in the trial
#' 
#' @importFrom methods is new slot<-
#' @export
setClass(
    "Trial",

    slots = list(
        trial_name = "character",
        breeding_program = "character",
        location = "character",
        year = "numeric",
        design_type = "character",
        description = "character",
        trial_type = "character",
        plot_width = "numeric",
        plot_length = "numeric",
        field_size = "numeric",
        planting_date = "character",
        harvest_date = "character",
        plots = "vector"
    ),

    prototype = list(
        trial_name = NA_character_,
        breeding_program = NA_character_,
        location = NA_character_,
        year = NA_integer_,
        design_type = NA_character_,
        description = NA_character_,
        trial_type = NA_character_,
        plot_width = NA_real_,
        plot_length = NA_real_,
        field_size = NA_real_,
        planting_date = NA_character_,
        harvest_date = NA_character_,
        plots = vector()
    ),

    validity = function(object) {
        
        # Required Slots
        if ( is.na(object@trial_name) ) {
            return("Trial Name is required")
        }
        if ( is.na(object@breeding_program) ) {
            return("Breeding Program is required")
        }
        if ( is.na(object@location) ) {
            return("Location is required")
        }
        if ( is.na(object@year) ) {
            return("Year is required")
        }
        if ( is.na(object@design_type) ) {
            return("Design Type is required")
        }
        if ( is.na(object@description) ) {
            return("Description is required")
        }

        # Check Values
        if ( !(object@design_type %in% DESIGN_TYPES) ) {
            return(paste0(
                "Design Type is not recognized. Supported Design Types: ", 
                paste(DESIGN_TYPES, collapse=", ")
            ))
        }
        if ( !is.na(object@trial_type) && (!object@trial_type %in% TRIAL_TYPES) ) {
            return(paste0(
                "Trial Type is not recognized. Supported Trial Types: ", 
                paste(TRIAL_TYPES, collapse=", ")
            ))
        }
        if ( !grepl("^[0-9]{4}$", object@year) ) {
            return("Invalid year. Required format: YYYY")
        }
        if ( !is.na(object@planting_date) && !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", object@planting_date) ) {
            return("Invalid planting date. Required format: YYYY-MM-DD")
        }
        if ( !is.na(object@harvest_date) && !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", object@harvest_date) ) {
            return("Invalid harvest date. Required format: YYYY-MM-DD")
        }
        for ( p in object@plots ) {
            if ( !is(p, "Plot") ) {
                return("All items in the list of plots must be an object of Class Plot")
            }
        }

        # Passed Validation
        return(TRUE)

    }

)


# List of supported Design Types
DESIGN_TYPES <- c('CRD', 'RCBD', 'Alpha', 'Augmented', 'MAD', 'Westcott', 'Lattice')

# List of supported Trial Types
TRIAL_TYPES <- c('Seedling Nursery', 'phenotyping_trial', 'Advanced Yield Trial', 'Preliminary Yield Trial', 'Uniform Yield Trial', 'Variety Release Trial', 'Clonal Evaluation', 'genetic_gain_trial', 'storage_trial', 'heterosis_trial', 'health_status_trial', 'grafting_trial', 'Screen House', 'Seed Multiplication', 'crossing_block_trial', 'Specialty Trial')