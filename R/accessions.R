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
#' See function \code{\link{getSupportedAccessionProperties}} for all supported accession property names
#' 
#' @seealso \link{getSupportedAccessionProperties}
#' 
#' @examples
#' accession <- Accession(
#'      "JERRY", 
#'      "Triticum aestivum",
#'      list(
#'          synonym = c("ND9257", "PI632433"),
#'          "institute code" = "NDSU",
#'          organization_name = "North Dakota State University"
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
        properties = properties
    )

    # Return the Accession
    return(accession)

}

#' Get Supported Accession Properties
#' 
#' Get a vector of accession property names that can be used by the Accession class
#' 
#' @return A vector of supported accession property names
#' 
#' @export
getSupportedAccessionProperties <- function() {
    return(c(
        getOption("breedbase.standard_stock_props"), 
        getOption("breedbase.editable_stock_props")
    ))
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
    
    # Get supported accession properties
    supported_props = getSupportedAccessionProperties()

    # Set template headers
    template <- tibble::tibble(
        "accession_name" = character(),
        "species_name" = character()
    )
    for ( prop in supported_props ) {
        template <- tibble::add_column(template, !!(prop) := NA_character_)
    }

    # Return blank template if no accessions provided
    if ( is.null(accessions) ) {
        return(template)
    }

    # Ensure a vector
    accessions <- c(accessions)

    # Add each of the Accessions
    for ( accession in accessions ) {

        # Create new row, with accession and species names set, empty columns for all stock props
        row <- tibble::tibble(
            "accession_name" = accession@accession_name,
            "species_name" = accession@species_name
        )
        for ( prop in supported_props ) {
            row <- tibble::add_column(row, !!(prop) := NA_character_)
        }

        # Set the stock prop values
        for ( prop in supported_props ) {
            value <- accession@properties[[prop]]
            if ( !is.null(value) ) {
                if ( is.vector(value) ) {
                    row[prop] <- paste(value, collapse=",")
                }
                else {
                    row[prop] <- value
                }
            }
            else {
                row[prop] <- NA
            }
        }

        # Add row to template
        template <- dplyr::bind_rows(template, row)

    }

    # Remove any unused editable stock props from the final template
    cols_to_remove <- c()
    for ( col in names(template) ) {
        if ( col %in% getOption("breedbase.editable_stock_props") ) {
            values <- unique(template[[col]])
            if ( length(values) == 1 && is.na(values) ) {
                cols_to_remove <- c(cols_to_remove, col)
            }
        }
    }
    filtered_template <- template
    if ( length(cols_to_remove) > 0 ) {
        filtered_template <- dplyr::select(template, -cols_to_remove)
    }

    # Return the template
    return(filtered_template)

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
#' @import dplyr WriteXLS
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
