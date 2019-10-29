#' Progeny Class
#' 
#' An S4 Class to represent a breeDBase progeny / pedigree
#' 
#' @slot progeny_name Name or synonym of the Accession to add pedigree information to (must exist in database)
#' @slot female_parent_accession Name of female parent Accession
#' @slot male_parent_accession Name of male parent Accession
#' @slot type Cross type (biparental, self, open, sib)
#' 
#' @importFrom methods is new slot<-
#' @export
setClass(
    "Progeny",

    slots = list(
        progeny_name = "character",
        female_parent_accession = "character",
        male_parent_accession = "character",
        type = "character"
    ),

    prototype = list(
        progeny_name = NA_character_,
        female_parent_accession = NA_character_,
        male_parent_accession = NA_character_,
        type = "biparental"
    ),

    validity = function(object) {
        if ( is.na(object@progeny_name) ) {
            return("Progeny Name is required")
        }
        if ( is.na(object@female_parent_accession) ) {
            return("Female Parent Accession is required")
        }
        if ( is.na(object@male_parent_accession) ) {
            return("Male Parent Accession is required")
        }

        if ( !(object@type %in% PROGENY_TYPES) ) {
            return("Progeny Type is not recognized")
        }

        return(TRUE)
    }

)

# List of approved progeny types
PROGENY_TYPES <- c("biparental", "self", "open", "sib")