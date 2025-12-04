#' Create PlotData
#' 
#' Create a PlotData object that contains phenotype observations for a single plot in a 
#' phenotyping trial.  The Plot Name and Observations are required.
#' 
#' The Observations take the form of a named list where the list item name / key is the 
#' full trait name, including the trait ID (ex: "Blade length cm|CO_360:0000240") and the 
#' list item value is the observed trait value (in the correct units).
#' 
#' @param plot_name The unique name for the plot (must already exist as a Plot in a Trial)
#' @param observations List of observations where the key is the trait name and the value is the observed trait value
#' @param notes (optional) Additional plot notes
#' 
#' @examples
#' plotdata1 <- PlotData("FARM-2019-UNH_PLOT1", list("Blade length cm|CO_360:0000240" = 24, "Blade width cm|CO_360:0000241" = 5))
#' plotdata2 <- PlotData("FARM-2019-UNH_PLOT2", list("Blade length cm|CO_360:0000240" = 27, "Blade width cm|CO_360:0000241" = 4))
#' 
#' @return PlotData
#' 
#' @family Trial
#' @export
PlotData <- function(
    plot_name = NULL,
    observations = list(),
    notes = NA_character_
) {

    # Check for required properties
    if ( is.null(plot_name) ) {
        stop("Cannot create PlotData: plot name required")
    }

    # Create the PlotData
    data <- new(
        "PlotData",
        plot_name = plot_name,
        observations = observations,
        notes = notes
    )

    # Return the PlotData
    return(data)

}


#' Build PlotData Template
#' 
#' Create a \code{tibble} representing the breeDBasae upload
#' template for the provided plotdata.  This will be a 'Simple' 
#' spreadsheet format with data recorded at the plot-level.
#' 
#' @param plotdata Vector of PlotData to add to the template
#' 
#' @return A \code{tibble} representation of the upload template
#' 
#' @family Trial
#' @import dplyr tibble
#' @export
buildPlotDataTemplate <- function(
    plotdata = NULL
) {

    # Set the template headers
    template <- tibble::tibble(
        "observationunit_name" = character()
    )

    # Return blank template if no plotdata provided
    if ( is.null(plotdata) ) {
        return(template)
    }

    # Ensure a vector
    plotdata <- c(plotdata)

    # Get all trait names
    traits <- c()
    for ( pd in plotdata ) {
        traits <- sort(unique(c(traits, names(pd@observations))))
    }

    # Parse each PlotData
    for ( pd in plotdata ) {
        row <- tibble::tibble(
            "observationunit_name" = pd@plot_name
        )
        for ( name in names(pd@observations) ) {
            row[[name]] <- pd@observations[[name]]
        }
        template <- dplyr::bind_rows(template, row)
    }

    # Clean template
    for ( name in names(template) ) {
        template[name][which(template[name] == ""),] <- NA
    }

    # Return the template
    return(template)

}



#' Write PlotData Template
#' 
#' Write a breeDBase upload template (.xls file) for plot data
#' 
#' @param input Either a vector of PlotDatas to include in the template OR 
#' a \code{tibble} representation of the upload template
#' @param output The file path to the output .xls file
#' 
#' @family Trial
#' @import WriteXLS
#' @export
writePlotDataTemplate <- function(
    input = NULL,
    output = NULL,
    chunk = NULL
) {

    # Check for required arguments
    if ( is.null(input) ) {
        stop("Cannot write Plot Data Template file: input of a template as a tibble or vector of PlotDatas is required")
    }
    if ( is.null(output) ) {
        stop("Cannot write Plot Data Template file: output of the file path to the .xls file is required")
    }

    # Create template if not provided one
    if ( !("tbl_df" %in% is(input)) ) {
        input <- buildPlotDataTemplate(input)
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
            writePlotDataTemplate(subset, subset_output)

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
        print(sprintf("Writing Plot Data Template: %s", output))
        WriteXLS::WriteXLS(input, output)
    }

}