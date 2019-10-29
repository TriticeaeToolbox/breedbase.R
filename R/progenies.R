#' Create Progeny
#' 
#' Create a Progeny / Pedigree for the Accession specified 
#' as `progeny` with the given female and male parents.
#' 
#' @param progeny The progeny Accession or Accession name (or synonym)
#' @param female_parent The female parent Accession or Accession name
#' @param male_parent The male parent Accession or Accession name
#' @param type (optional, default = 'biparental') The cross type (biparental, self, open, sib)
#' 
#' @examples
#' # Create a Progeny from names
#' progeny1 <- Progeny('SL18-UCONN-S1', 'SA18-CB-S10-FG1', 'SL18-SF-S19-MG2')
#' 
#' # Create a Progeny from existing Accessions
#' offspring <- Accession('SL18-UCONN-S1', 'Saccharina latissima')
#' mother <- Accession('SA18-CB-S10-FG1', 'Saccharina angustissima')
#' father <- Accession('SL18-SF-S19-MG2', 'Saccharina latissima')
#' progeny2 <- Progeny(offspring, mother, father)
#' 
#' @return Progeny
#' 
#' @export
Progeny <- function(
    progeny = NULL,
    female_parent = NULL,
    male_parent = NULL,
    type = "biparental"
) {

    # Check for required properties
    if ( is.null(progeny) ) {
        stop("Cannot create Progent: progeny Accession or name required")
    }
    if ( is.null(female_parent) ) {
        stop("Cannot create Progent: female parent Accession or name required")
    }
    if ( is.null(male_parent) ) {
        stop("Cannot create Progent: male parent Accession or name required")
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

    # Create the Progeny
    rtn <- new(
        "Progeny",
        progeny_name = progeny,
        female_parent_accession = female_parent,
        male_parent_accession = male_parent,
        type = type
    )

    # Return the Progeny
    return(rtn)

}


#' Build Progeny Template
#' 
#' Create a \code{tibble} representing the breeDBase upload 
#' template for the provided progenies
#' 
#' @param progenies Vector of Progenies to add to the template
#' 
#' @return A \code{tibble} representation of the upload template
#' 
#' @import dplyr tibble
#' @export
buildProgenyTemplate <- function(
    progenies = NULL
) {

    # Set template headers
    template <- tibble::tibble(
        "progeny name" = character(),
        "female parent accession" = character(),
        "male parent accession" = character(),
        "type" = character()
    )

    # Return blank template if no progenies provided
    if ( is.null(progenies) ) {
        return(template)
    }

    # Ensure a vector
    progenies <- c(progenies)

    # Parse each progeny
    for ( progeny in progenies ) {
        row <- tibble::tibble(
            "progeny name"  = progeny@progeny_name,
            "female parent accession" = progeny@female_parent_accession,
            "male parent accession" = progeny@male_parent_accession,
            "type" = progeny@type
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


#' Write Progeny Template
#' 
#' Write a breeDBase upload template (.txt tab delimited file) for progenies
#' 
#' @param input Either a vector of Progenies to include in the template OR 
#' a \code{tibble} representation of the upload template
#' @param output The file path to the output .txt file
#' @param chunk Chunk the file into parts with up to `chunk` number of lines per file
#' 
#' @export
writeProgenyTemplate <- function(
    input = NULL,
    output = NULL,
    chunk = NULL
) {

    # Check for required arguments
    if ( is.null(input) ) {
        stop("Cannot write Progeny Template file: input of a template as a tibble or vector of progenies is required")
    }
    if ( is.null(output) ) {
        stop("Cannot write Progeny Template file: output of the file path to the .txt file is required")
    }

    # Create template if not provided one
    if ( !("tbl_df" %in% is(input)) ) {
        input <- buildProgenyTemplate(input)
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
            writeProgenyTemplate(subset, subset_output)

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
        print(sprintf("Writing Progeny Template: %s", output))
        write.table(input, file = output, sep = "\t", quote = FALSE, na = "", row.names = FALSE, col.names = TRUE)
    }

}