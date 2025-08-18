#' Get Supported Accession Properties
#' 
#' Get a vector of accession property names that can be used by the Accession class.
#' These values can be overriden by setting the global \code{breedbase.editable_stock_props}
#' option (Example: \code{options("breedbase.editable_stock_props" = c("prop1", "prop2"))}).
#' 
#' @return A vector of supported accession property names
#' 
#' @family Accession
#' @export
getSupportedAccessionProperties <- function() {
    return(c(
        getBBOption("standard_stock_props"), 
        getBBOption("editable_stock_props")
    ))
}

#' Get the Accession Search Server
#' 
#' Get the set URL for the BrAPI Accession Search server.  This value 
#' can be overriden by setting the global \code{breedbase.accession_search_server}
#' option (Example: \code{options("breedbase.accession_search_server" = "https://search.example.org")}).
#' 
#' @return The URL to the Acccession Search server
#' 
#' @family accessionSearch
#' @export
getAccessionSearchServer <- function() {
    return(getBBOption("accession_search_server"))
}

#' Get the Accession Search Configuration
#' 
#' Get the default configuration properties for the accession search 
#' function.  The default properties can be overriden by setting the 
#' global \code{breedbase.accession_search_config} option (Example: 
#' \code{options("breedbase.accession_search_config" = config)}).
#' 
#' @return a nested list containing the default accession search properties
#' 
#' @family accessionSearch
#' @export
getAccessionSearchConfig <- function() {
    return(getBBOption("accession_search_config"))
}

#' Get the Country Codes
#' 
#' Get a vector of supported Location country codes.  This value can 
#' be overridden by setting the global \code{breedbase.country_codes} 
#' option (Example: \code{options("breedbase.country_codes" = country_codes)}).
#' 
#' @return A vector of country codes
#' 
#' @family Location
#' @export
getCountryCodes <- function() {
    return(getBBOption("country_codes"))
}

#' Get Location Types
#' 
#' Get a vector of supported Location types.  This value can be 
#' overridden by setting the global \code{breedbase.location_types} 
#' option (Example: \code{options("breedbase.location_types" = location_types)}).
#' 
#' @return A vector of location types
#' 
#' @family Location
#' @export
getLocationTypes <- function() {
    return(getBBOption("location_types"))
}

#' Get Trial Design Types
#' 
#' Get a vector of supported Trial design types.  This value can be 
#' overridden by setting the global \code{breedbase.design_types} 
#' option (Example: \code{options("breedbase.design_types" = design_types)}).
#' 
#' @return A vector of Trial design types
#' 
#' @family Trial
#' @export
getTrialDesignTypes <- function() {
    return(getBBOption("design_types"))
}

#' Get Trial Types
#' 
#' Get a vector of supported Trial types.  This value can be 
#' overridden by setting the global \code{breedbase.trial_types} 
#' option (Example: \code{options("breedbase.trial_types" = trial_types)}).
#' 
#' @return A vector of Trial types
#' 
#' @family Trial
#' @export
getTrialTypes <- function() {
    return(getBBOption("trial_types"))
}



## ======== PRIVATE HELPER FUNCTIONS ======== ##

# Get the value of a breedbase option, either a user configured global 
# option, if set, or the default value
# @param key The name of the breedbase option (without the breedbase prefix)
# @returns The value of the option from the global options, if set, or the default value
getBBOption <- function(key) {
    return(
        getOption(
            paste("breedbase", key, sep="."), 
            DEFAULT_BB_OPTIONS[[key]]
        )
    )
}



## ======== DEFAULT OPTION VALUES ======== ##

DEFAULT_BB_OPTIONS = list(
    standard_stock_props = c("population_name", "organization_name", "synonym", "PUI"),
    editable_stock_props = c("variety", "released_variety_name", "donor", "donor institute", "donor PUI", "country of origin", "state", "institute code", "institute name", "biological status of accession code", "notes", "accession number", "seed source", "type of germplasm storage code", "acquisition date", "location_code", "ploidy_level", "genome_structure", "ncbi_taxonomy_id", "transgenic", "introgression_parent", "introgression_backcross_parent", "introgression_map_version", "introgression_chromosome", "introgression_start_position_bp", "introgression_end_position_bp", "purdy_pedigree", "filial_generation", "female_parent", "male_parent", "cross_type"),
    cross_types = c("biparental", "self", "open", "backcross", "sib", "polycross", "reselected"),
    country_codes = c('AFG','ALA','ALB','DZA','ASM','AND','AGO','AIA','ATA','ATG','ARG','ARM','ABW','AUS','AUT','AZE','BHS','BHR','BGD','BRB','BLR','BEL','BLZ','BEN','BMU','BTN','BOL','BES','BIH','BWA','BVT','BRA','IOT','VGB','BRN','BGR','BFA','BDI','CPV','KHM','CMR','CAN','CYM','CAF','TCD','CHL','CHN','HKG','MAC','CXR','CCK','COL','COM','COG','COK','CRI','CIV','HRV','CUB','CUW','CYP','CZE','PRK','COD','DNK','DJI','DMA','DOM','ECU','EGY','SLV','GNQ','ERI','EST','SWZ','ETH','FLK','FRO','FJI','FIN','FRA','GUF','PYF','ATF','GAB','GMB','GEO','DEU','GHA','GIB','GRC','GRL','GRD','GLP','GUM','GTM','GGY','GIN','GNB','GUY','HTI','HMD','VAT','HND','HUN','ISL','IND','IDN','IRN','IRQ','IRL','IMN','ISR','ITA','JAM','JPN','JEY','JOR','KAZ','KEN','KIR','KWT','KGZ','LAO','LVA','LBN','LSO','LBR','LBY','LIE','LTU','LUX','MDG','MWI','MYS','MDV','MLI','MLT','MHL','MTQ','MRT','MUS','MYT','MEX','FSM','MCO','MNG','MNE','MSR','MAR','MOZ','MMR','NAM','NRU','NPL','NLD','NCL','NZL','NIC','NER','NGA','NIU','NFK','MKD','MNP','NOR','OMN','PAK','PLW','PAN','PNG','PRY','PER','PHL','PCN','POL','PRT','PRI','QAT','KOR','MDA','REU','ROU','RUS','RWA','BLM','SHN','KNA','LCA','MAF','SPM','VCT','WSM','SMR','STP','SAU','SEN','SRB','SYC','SLE','SGP','SXM','SVK','SVN','SLB','SOM','ZAF','SGS','SSD','ESP','LKA','PSE','SDN','SUR','SJM','SWE','CHE','SYR','TJK','THA','TLS','TGO','TKL','TON','TTO','TUN','TUR','TKM','TCA','TUV','UGA','UKR','ARE','GBR','TZA','UMI','USA','VIR','URY','UZB','VUT','VEN','VNM','WLF','ESH','YEM','ZMB','ZWE'),
    location_types = c('Town', 'Farm', 'Field', 'Greenhouse', 'Screenhouse', 'Lab', 'Storage', 'Other'),
    design_types = c('CRD', 'RCBD', 'RRC', 'Doubly-Resolvable Row-Column', 'Augmented Row-Column', 'Alpha', 'Lattice', 'Augmented', 'MAD', 'greenhouse', 'splitplot', 'p-rep', 'Westcott'),
    trial_types = c('Seedling Nursery', 'phenotyping_trial', 'Advanced Yield Trial', 'Preliminary Yield Trial', 'Uniform Yield Trial', 'Variety Release Trial', 'Clonal Evaluation', 'genetic_gain_trial', 'storage_trial', 'heterosis_trial', 'health_status_trial', 'grafting_trial', 'Screen House', 'Seed Multiplication', 'crossing_block_trial', 'Specialty Trial'),
    accession_search_server = "https://synonyms.triticeaetoolbox.org",
    accession_search_config = list(
        database_terms = list(name = TRUE, synonyms = TRUE, accession_numbers = TRUE), 
        search_routines = list(exact = TRUE, punctuation = TRUE, substring = TRUE, prefix = FALSE, edit_distance = FALSE),
        search_routine_options = list(
            substring = list(substring_length_min = 3),
            prefix = list(prefixes = c(), find_db_prefixes = TRUE, prefix_length_min = 2, prefix_length_max = 5, threshold = 250),
            edit_distance = list(max_edit_distance = 2)
        ),
        case_sensitive = TRUE,
        return_records = FALSE
    )
)