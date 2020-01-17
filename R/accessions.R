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
        species_name = species_name,
        population_name = if(is.null(properties$population_name)) NA_character_ else properties$population_name,
        organization_names = if(is.null(properties$organization_names)) vector() else properties$organization_names,
        synonyms = if(is.null(properties$synonyms)) vector() else properties$synonyms,
        location_codes = if(is.null(properties$location_codes)) vector() else properties$location_codes,
        ploidy_levels = if(is.null(properties$ploidy_levels)) vector() else properties$ploidy_levels,
        genome_structures = if(is.null(properties$genome_structures)) vector() else properties$genome_structures,
        varietys = if(is.null(properties$varietys)) vector() else properties$varietys,
        donors = if(is.null(properties$donors)) vector() else properties$donors,
        donor_institutes = if(is.null(properties$donor_institutes)) vector() else properties$donor_institutes,
        donor_PUIs = if(is.null(properties$donor_PUIs)) vector() else properties$donor_PUIs,
        country_of_origins = if(is.null(properties$country_of_origins)) vector() else properties$country_of_origins,
        states = if(is.null(properties$states)) vector() else properties$states,
        institute_codes = if(is.null(properties$institute_codes)) vector() else properties$institute_codes,
        institute_names = if(is.null(properties$institute_names)) vector() else properties$institute_names,
        biological_status_of_accession_codes = if(is.null(properties$biological_status_of_accession_codes)) vector() else properties$biological_status_of_accession_codes,
        notes = if(is.null(properties$notes)) vector() else properties$notes,
        accession_numbers = if(is.null(properties$accession_numbers)) vector() else properties$accession_numbers,
        PUIs = if(is.null(properties$PUIs)) vector() else properties$PUIs,
        seed_sources = if(is.null(properties$seed_sources)) vector() else properties$seed_sources,
        type_of_germplasm_storage_codes = if(is.null(properties$type_of_germplasm_storage_codes)) vector() else properties$type_of_germplasm_storage_codes,
        acquisition_dates = if(is.null(properties$acquisition_dates)) vector() else properties$acquisition_dates,
        transgenic = if(is.null(properties$transgenic)) NA_integer_ else properties$transgenic,
        introgression_parent = if(is.null(properties$introgression_parent)) NA_character_ else properties$introgression_parent,
        introgression_backcross_parent = if(is.null(properties$introgression_backcross_parent)) NA_character_ else properties$introgression_backcross_parent,
        introgression_map_version = if(is.null(properties$introgression_map_version)) NA_character_ else properties$introgression_map_version,
        introgression_chromosome = if(is.null(properties$introgression_chromosome)) NA_character_ else properties$introgression_chromosome,
        introgression_start_position_bp = if(is.null(properties$introgression_start_position_bp)) NA_real_ else properties$introgression_start_position_bp,
        introgression_end_position_bp = if(is.null(properties$introgression_end_position_bp)) NA_real_ else properties$introgression_end_position_bp
    )

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

    # Return blank template if no accessions provided
    if ( is.null(accessions) ) {
        return(template)
    }

    # Ensure a vector
    accessions <- c(accessions)

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
        template[name][which(template[name] == ""),] <- NA
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
#' @param chunk Chunk the file into parts with up to `chunk` number of lines per file
#' 
#' @import WriteXLS
#' @export
writeAccessionTemplate <- function(
    input = NULL,
    output = NULL,
    chunk = NULL
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
            writeAccessionTemplate(subset, subset_output)

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
        print(sprintf("Writing Accession Template: %s", output))
        WriteXLS::WriteXLS(input, output)
    }

}

