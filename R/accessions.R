#' Create Accession
#' 
#' Create an Accession containing all of the provided properties. 
#' Accession Name and Species Name are required.  All optional 
#' properties can be added as a named value in the properties list.
#' 
#' @param accession_name Accession Name
#' @param species_name Species Name
#' @param properties (optional) Additional accession properties (as a named list)
#' 
#' See Class \linkS4class{Accession} for all optional accession properties
#' 
#' @examples
#' accession <- Accession(
#'      "JERRY", 
#'      "Triticum aestivum",
#'      list(
#'          synonyms = c("ND9257", "PI632433"),
#'          institute_codes = "NDSU",
#'          organization_names = "North Dakota State University"
#'      )
#' )
#' 
#' @return Accession
#' 
#' @export
Accession <- function(
    accession_name = NULL,
    species_name = NULL,
    properties = list()
) {

    # Check for required properties
    if ( is.null(accession_name) ) {
        stop("Cannot create Accession: accession name required")
    }
    if ( is.null(species_name) ) {
        stop("Cannot create Accession: species name required")
    }

    # Create Accession with required properties
    accession <- new(
        "Accession",
        accession_name = accession_name,
        species_name = species_name
    )

    # Add optional properties
    for ( name in names(properties) ) {
        value <- unname(properties[name])[[1]]
        slot(accession, name) <- value
    }

    # Return the Accession
    return(accession)

}



#' Build Accession Template
#' 
#' Create a \code{tibble} representing the breeDBase upload 
#' template for the provided accessions
#' 
#' @param accessions Vector of Accessions to add to the template
#' 
#' @return A \code{tibble} representation of the upload template
#' 
#' @import dplyr tibble
#' @export
buildAccessionTemplate <- function(
    accessions = NULL
) {

    # Check for required arguments
    if ( is.null(accessions) ) {
        stop("Cannot create Accession Template: vector of accessions required")
    }

    # Set template headers
    template <- tibble::tibble(
        "accession_name" = character(),
        "species_name" = character(),
        "population_name" = character(),
        "organization_name(s)" = vector(),
        "synonym(s)" = vector(),
        "location_code(s)" = vector(),
        "ploidy_level(s)" = vector(),
        "genome_structure(s)" = vector(),
        "variety(s)" = vector(),
        "donor(s)" = vector(),
        "donor_institute(s)" = vector(),
        "donor_PUI(s)" = vector(),
        "country_of_origin(s)" = vector(),
        "state(s)" = vector(),
        "institute_code(s)" = vector(),
        "institute_name(s)" = vector(),
        "biological_status_of_accession_code(s)" = vector(),
        "notes(s)" = vector(),
        "accession_number(s)" = vector(),
        "PUI(s)" = vector(),
        "seed_source(s)" = vector(),
        "type_of_germplasm_storage_code(s)" = vector(),
        "acquisition_date(s)" = vector(),
        "transgenic" = numeric(),
        "introgression_parent" = character(),
        "introgression_backcross_parent" = character(),
        "introgression_map_version" = character(),
        "introgression_chromosome" = character(),
        "introgression_start_position_bp" = numeric(),
        "introgression_end_position_bp" = numeric()
    )

    # Add each of the Accessions
    for ( accession in accessions ) {
        row <- tibble::tibble(
            "accession_name" = accession@accession_name,
            "species_name" = accession@species_name,
            "population_name" = accession@population_name,
            "organization_name(s)" = paste(accession@organization_names, collapse=","),
            "synonym(s)" = paste(accession@synonyms, collapse=","),
            "location_code(s)" = paste(accession@location_codes, collapse=","),
            "ploidy_level(s)" = paste(accession@ploidy_levels, collapse=","),
            "genome_structure(s)" = paste(accession@genome_structures, collapse=","),
            "variety(s)" = paste(accession@varietys, collapse=","),
            "donor(s)" = paste(accession@donors, collapse=","),
            "donor_institute(s)" = paste(accession@donor_institutes, collapse=","),
            "donor_PUI(s)" = paste(accession@donor_PUIs, collapse=","),
            "country_of_origin(s)" = paste(accession@country_of_origins, collapse=","),
            "state(s)" = paste(accession@states, collapse=","),
            "institute_code(s)" = paste(accession@institute_codes, collapse=","),
            "institute_name(s)" = paste(accession@institute_names, collapse=","),
            "biological_status_of_accession_code(s)" = paste(accession@biological_status_of_accession_codes, collapse=","),
            "notes(s)" = paste(accession@notes, collapse=","),
            "accession_number(s)" = paste(accession@accession_numbers, collapse=","),
            "PUI(s)" = paste(accession@PUIs, collapse=","),
            "seed_source(s)" = paste(accession@seed_sources, collapse=","),
            "type_of_germplasm_storage_code(s)" = paste(accession@type_of_germplasm_storage_codes, collapse=","),
            "acquisition_date(s)" = paste(accession@acquisition_dates, collapse=","),
            "transgenic" = accession@transgenic,
            "introgression_parent" = accession@introgression_parent,
            "introgression_backcross_parent" = accession@introgression_backcross_parent,
            "introgression_map_version" = accession@introgression_map_version,
            "introgression_chromosome" = accession@introgression_chromosome,
            "introgression_start_position_bp" = accession@introgression_start_position_bp,
            "introgression_end_position_bp" = accession@introgression_end_position_bp
        )
        template <- dplyr::bind_rows(template, row)
    }

    # Clean template
    for ( name in names(template) ) {
        template[name][which(template[name] == ""),] = NA
    }

    # Return the template
    return(template)

}


#' Write Accession Template
#' 
#' Write a breeDBase upload template (.xls file) for accessions
#' 
#' @param input Either a vector of Accessions to include in the template OR 
#' a \code{tibble} representation of the upload template
#' @param output The file path to the output .xls file
#' 
#' @import WriteXLS
#' @export
writeAccessionTemplate <- function(
    input=NULL,
    output=NULL
) {

    # Check for required arguments
    if ( is.null(input) ) {
        stop("Cannot write Accession Template file: input of a template as a tibble or vector of accessions is required")
    }
    if ( is.null(output) ) {
        stop("Cannot write Accession Template file: output of the file path to the .xls file is required")
    }

    # Create template if not provided one
    if ( !("tbl_df" %in% is(input)) ) {
        input <- buildAccessionTemplate(input)
    }

    # Set output extension
    if ( !grepl("\\.xls$", output) ) {
        output = paste(output, ".xls", sep="")
    }

    # Write Excel File
    WriteXLS::WriteXLS(input, output)

}

