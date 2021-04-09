#' Create Pedigree
#' 
#' Create a Pedigree for the Accession specified as `progeny` 
#' with the given female and male parents.
#' 
#' @param progeny The progeny Accession or Accession name (or synonym)
#' @param female_parent The female parent Accession or Accession name
#' @param male_parent The male parent Accession or Accession name
#' @param type (optional, default = 'biparental') The cross type (biparental, self, open, sib)
#' 
#' @examples
#' # Create a Pedigree from names
#' pedigree1 <- Pedigree('SL18-UCONN-S1', 'SA18-CB-S10-FG1', 'SL18-SF-S19-MG2')
#' 
#' # Create a Pedigree from existing Accessions
#' offspring <- Accession('SL18-UCONN-S1', 'Saccharina latissima')
#' mother <- Accession('SA18-CB-S10-FG1', 'Saccharina angustissima')
#' father <- Accession('SL18-SF-S19-MG2', 'Saccharina latissima')
#' pedigree2 <- Pedigree(offspring, mother, father)
#' 
#' @return Pedigree
#' 
#' @family Accession
#' @export
Pedigree <- function(
    progeny = NULL,
    female_parent = NULL,
    male_parent = NULL,
    type = "biparental"
) {

    # Check for required properties
    if ( is.null(progeny) ) {
        stop("Cannot create Pedigree: progeny Accession or name required")
    }
    if ( is.null(female_parent) ) {
        stop("Cannot create Pedigree: female parent Accession or name required")
    }
    if ( is.null(male_parent) ) {
        stop("Cannot create Pedigree: male parent Accession or name required")
    }

    # Set names, if Accessions
    if ( is(progeny, "Accession") ) {
        progeny <- progeny@accession_name
    }
    if ( is(female_parent, "Accession") ) {
        female_parent <- female_parent@accession_name
    }
    if ( is(male_parent, "Accession") ) {
        male_parent <- male_parent@accession_name
    }

    # Create the Pedigree
    rtn <- new(
        "Pedigree",
        progeny_name = progeny,
        female_parent_accession = female_parent,
        male_parent_accession = male_parent,
        type = type
    )

    # Return the Pedigree
    return(rtn)

}


#' Build Pedigree Template
#' 
#' Create a \code{tibble} representing the breeDBase upload 
#' template for the provided pedigrees
#' 
#' @param pedigrees Vector of Pedigrees to add to the template
#' 
#' @return A \code{tibble} representation of the upload template
#' 
#' @family Accession
#' @import dplyr tibble
#' @export
buildPedigreeTemplate <- function(
    pedigrees = NULL
) {

    # Set template headers
    template <- tibble::tibble(
        "progeny name" = character(),
        "female parent accession" = character(),
        "male parent accession" = character(),
        "type" = character()
    )

    # Return blank template if no pedigrees provided
    if ( is.null(pedigrees) ) {
        return(template)
    }

    # Ensure a vector
    pedigrees <- c(pedigrees)

    # Parse each pedigree
    for ( pedigree in pedigrees ) {
        row <- tibble::tibble(
            "progeny name"  = pedigree@progeny_name,
            "female parent accession" = pedigree@female_parent_accession,
            "male parent accession" = pedigree@male_parent_accession,
            "type" = pedigree@type
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


#' Write Pedigree Template
#' 
#' Write a breeDBase upload template (.txt tab delimited file) for pedigrees
#' 
#' @param input Either a vector of Pedigrees to include in the template OR 
#' a \code{tibble} representation of the upload template
#' @param output The file path to the output .txt file
#' @param chunk Chunk the file into parts with up to `chunk` number of lines per file
#' 
#' @family Accession
#' @export
writePedigreeTemplate <- function(
    input = NULL,
    output = NULL,
    chunk = NULL
) {

    # Check for required arguments
    if ( is.null(input) ) {
        stop("Cannot write Pedigree Template file: input of a template as a tibble or vector of pedigrees is required")
    }
    if ( is.null(output) ) {
        stop("Cannot write Pedigree Template file: output of the file path to the .txt file is required")
    }

    # Create template if not provided one
    if ( !("tbl_df" %in% is(input)) ) {
        input <- buildPedigreeTemplate(input)
    }

    # Set output extension
    if ( !grepl("\\.txt$", output) ) {
        output <- paste(output, ".txt", sep="")
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
            writePedigreeTemplate(subset, subset_output)

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
        print(sprintf("Writing Pedigree Template: %s", output))
        write.table(input, file = output, sep = "\t", quote = FALSE, na = "", row.names = FALSE, col.names = TRUE)
    }

}