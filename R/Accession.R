#' Accession Class
#' 
#' An S4 Class to represent a breeDBase accession
#' 
#' The `accession_name` and `species_name` are required, all other fields are optional.
#' 
#' @slot accession_name Accession Name (must be unique)
#' @slot species_name Species Name (must exist in the database)
#' @slot population_name (optional) a population is a grouping of accessions. if the population already exists in the database, the accession will be added into it, otherwise, the new population will be created.
#' @slot organization_names (optional) the names of the organizations which use this accession.
#' @slot synonyms (optional) an accession can be known by many names including local popular names. a synonym name can be used instead of the accession_name throughout the database; because of this, synonyms must themselves be unique.
#' @slot location_codes (optional) location codes for the accession.
#' @slot ploidy_levels (optional) a number indicating the ploidy (e.g. 2 for diploid, 3 for triploid), numeric.
#' @slot genome_structures (optional) genome structures for accession which take into account ploidy and ancestral genome info e.g. AAA, AB.
#' @slot varietys (optional) variety can be defined as a group of individuals or plants having similar traits that can be reproduced "true to type" from generation to generation.
#' @slot donors (optional) the accession_name of the donor accession.
#' @slot donor_institutes (optional) the institute of the donor accession.
#' @slot donor_PUIs (optional) the permanent unique identifier of the donor accession.
#' @slot country_of_origins (optional) the country of origin.
#' @slot states (optional) the state of origin.
#' @slot institute_codes (optional) the institute code of origin.
#' @slot institute_names (optional) the institute name of origin.
#' @slot biological_status_of_accession_codes (optional) code indicating the state of accession.
#' @slot notes (optional) free text for notes.
#' @slot accession_numbers (optional) accession number for accession from germplasm bank.
#' @slot PUIs (optional) permanent unique identifier of the accession.
#' @slot seed_sources (optional) origin of seed source.
#' @slot type_of_germplasm_storage_codes (optional) code indicating the type of germplasm storage.
#' @slot acquisition_dates (optional) dates of acquisition YYYYMMDD for accession.
#' @slot transgenic (optional) indicates if accession is transgenic. Please indicate 1 if transgenic and empty if otherwise.
#' @slot introgression_parent (optional) if the accession you are adding has an introgression that originated from one of the parents, you can specify the parent here
#' @slot introgression_backcross_parent (optional) the backcross parent for introducing an introgression into the accession being added
#' @slot introgression_map_version (optional) the map version for identifying the start and stop position of the introgression. e.g. AGPv2
#' @slot introgression_chromosome (optional) the chromosome number that the introgression is on.
#' @slot introgression_start_position_bp (optional) the start position of the introgression in base pairs
#' @slot introgression_end_position_bp (optional) the end position of the introgression in base pairs
#' 
#' @importFrom methods is new slot<-
#' @export
setClass(
    "Accession",

    slots = list(
        accession_name = "character",
        species_name = "character",
        population_name = "character",
        organization_names = "vector",
        synonyms = "vector",
        location_codes = "vector",
        ploidy_levels = "vector",
        genome_structures = "vector",
        varietys = "vector",
        donors = "vector",
        donor_institutes = "vector",
        donor_PUIs = "vector",
        country_of_origins = "vector",
        states = "vector",
        institute_codes = "vector",
        institute_names = "vector",
        biological_status_of_accession_codes = "vector",
        notes = "vector",
        accession_numbers = "vector",
        PUIs = "vector",
        seed_sources = "vector",
        type_of_germplasm_storage_codes = "vector",
        acquisition_dates = "vector",
        transgenic = "numeric",
        introgression_parent = "character",
        introgression_backcross_parent = "character",
        introgression_map_version = "character",
        introgression_chromosome = "character",
        introgression_start_position_bp = "numeric",
        introgression_end_position_bp = "numeric"
    ),

    prototype = list(
        accession_name = NA_character_,
        species_name = NA_character_,
        population_name = NA_character_,
        organization_names = vector(),
        synonyms = vector(),
        location_codes = vector(),
        ploidy_levels = vector(),
        genome_structures = vector(),
        varietys = vector(),
        donors = vector(),
        donor_institutes = vector(),
        donor_PUIs = vector(),
        country_of_origins = vector(),
        states = vector(),
        institute_codes = vector(),
        institute_names = vector(),
        biological_status_of_accession_codes = vector(),
        notes = vector(),
        accession_numbers = vector(),
        PUIs = vector(),
        seed_sources = vector(),
        type_of_germplasm_storage_codes = vector(),
        acquisition_dates = vector(),
        transgenic = NA_integer_,
        introgression_parent = NA_character_,
        introgression_backcross_parent = NA_character_,
        introgression_map_version = NA_character_,
        introgression_chromosome = NA_character_,
        introgression_start_position_bp = NA_real_,
        introgression_end_position_bp = NA_real_
    ),
    
    validity = function(object) {
        if ( is.na(object@accession_name) ) {
            return("Accession Name is required")
        }
        if ( is.na(object@species_name) ) {
            return("Species Name is required")
        }
        return(TRUE)
    }
)