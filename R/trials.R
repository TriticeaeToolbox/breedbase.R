#' Create Trial
#' 
#' Create a phenotyping Trial containing all of the provided  properties.  Trial Name, 
#' Breeding Program, Location, Year, Design Type and Description are required.  All 
#' optional properties can be added as a named value in the properties list.
#' 
#' See Class \linkS4class{Trial} for all optional trial properties
#' 
#' Use the \code{\link{getTrialDesignTypes}} function to get a list of supported Trial design types and the 
#' \code{\link{getTrialTypes}} function to get a list of the supported Trial types.
#' 
#' @param trial_name A name for the trial (must be unique across the entire database)
#' @param breeding_program The name of the breeding program that managed the trial (must exist in database)
#' @param location The name of the location where the trial was held (must exist in database)
#' @param year The year the trial was held
#' @param design_type The shorthand for the design type (CRD, RCBD, Alpha, Augmented, MAD, Westcott, Lattice)
#' @param description Additional text with any other relevant information about the trial
#' @param properties (optional) Additional trial properties (as a named list)
#' 
#' @examples
#' # Create a Trial with just the required parameters
#' trial1 <- Trial("UMOPN_2019_Madison", "University of Wisconsin", "Madison, WI", 2019, "RCBD", "UMOPN Nursery Trial")
#' 
#' # Create a Trial with optional parameters
#' opts <- list(planting_date = "2019-04-25", harvest_date = "2019-10-05", trial_type = "phenotyping_trial")
#' trial2 <- Trial("UMOPN_2019_Arlington", "University of Wisconsin", "Arlington, WI", 2019, "RCBD", "UMOPN Nursery Trial", opts)
#' 
#' @return Trial
#' 
#' @family Trial
#' @export
Trial <- function(
    trial_name = NULL,
    breeding_program = NULL,
    location = NULL,
    year = NULL,
    design_type = NULL,
    description = NULL,
    properties = list()
) {

    # Check for required properties
    if ( is.null(trial_name) ) {
        stop("Cannot create Trial: trial name required")
    }
    if ( is.null(breeding_program) ) {
        stop("Cannot create Trial: breeding program required")
    }
    if ( is.null(location) ) {
        stop("Cannot create Trial: location name required")
    }
    if ( is.null(year) ) {
        stop("Cannot create Trial: year required")
    }
    if ( is.null(design_type) ) {
        stop("Cannot create Trial: design type required")
    }
    if ( is.null(description) ) {
        stop("Cannot create Trial: description required")
    }

    # Create Trial with required properties
    trial <- new(
        "Trial",
        trial_name = trial_name,
        breeding_program = breeding_program,
        location = location,
        year = year,
        design_type = design_type,
        description = description,
        trial_type = if(is.null(properties$trial_type)) NA_character_ else properties$trial_type,
        plot_width = if(is.null(properties$plot_width)) NA_real_ else properties$plot_width,
        plot_length = if(is.null(properties$plot_length)) NA_real_ else properties$plot_length,
        field_size = if(is.null(properties$field_size)) NA_real_ else properties$field_size,
        planting_date = if(is.null(properties$planting_date)) NA_character_ else properties$planting_date,
        harvest_date = if(is.null(properties$harvest_date)) NA_character_ else properties$harvest_date,
        plots = if(is.null(properties$plots)) vector() else properties$plots
    )
    
    # Return the Trial
    return(trial)

}


#' Set Trial Plots
#' 
#' Set the provided vector of Plots as the `plots` property 
#' of the provided Trial
#' 
#' @param trial A Trial to add the plots to
#' @param plots A vector of Plots to add to the Trial
#' 
#' @return Trial
#' 
#' @family Trial
#' @export
setTrialPlots <- function(
    trial = NULL,
    plots = NULL
) {

    # Check required params
    if ( is.null(trial) ) {
        stop("Cannot set plots: trial is required")
    }
    if ( is.null(plots) ) {
        stop("Cannot set plots: plots are required")
    }

    # Check Trial and Plot types
    if ( !is(trial, "Trial") ) {
        stop("Trial must be an object of Class Trial")
    }
    for ( p in plots ) {
        if ( !is(p, "Plot") ) {
            stop("All items in the plots list must be an object of Class Plot")
        }
    }

    # Add plots to Trial
    slot(trial, "plots") <- plots
    return(trial)

}


#' Build Trial Template
#' 
#' Create a \code{tibble} representing the breeDBase upload 
#' template for the provided trials.  The Trial must have 
#' plots set in order to add it to the template.
#' 
#' @param trials Vector of Trials to add to the template
#' 
#' @return A \code{tibble} representation of the upload template
#' 
#' @family Trial
#' @import dplyr tibble
#' @export
buildTrialTemplate <- function(
    trials = NULL
) {

    # Set template headers
    template <- tibble::tibble(
        "trial_name" = character(),
        "breeding_program" = character(),
        "location" = character(),
        "year" = numeric(),
        "design_type" = character(),
        "description" = character(),
        "trial_type" = character(),
        "plot_width" = numeric(),
        "plot_length" = numeric(),
        "field_size" = numeric(),
        "planting_date" = character(),
        "harvest_date" = character(),
        "plot_name" = character(),
        "accession_name" = character(),
        "plot_number" = numeric(),
        "block_number" = numeric(),
        "is_a_control" = logical(),
        "rep_number" = numeric(),
        "range_number" = numeric(),
        "row_number" = numeric(),
        "col_number" = numeric(),
        "seedlot_name" = character(),
        "num_seed_per_plot" = numeric(),
        "weight_gram_seed_per_plot" = numeric()
    )

    # Return blank template if no trials provided
    if ( is.null(trials) ) {
        return(template)
    }

    # Ensure a vector
    trials <- c(trials)

    # Parse each Trial
    for ( trial in trials ) {

        # Get the plots
        plots <- trial@plots
        if ( length(plots) == 0 ) {
            print(sprintf("Warning: Trial %s has no plots ... skipping", trial@trial_name))
        }

        # Parse each Plot
        for ( plot in plots ) {

            # Create the Template Row
            row <- tibble::tibble(
                "trial_name" = trial@trial_name,
                "breeding_program" = trial@breeding_program,
                "location" = trial@location,
                "year" = trial@year,
                "design_type" = trial@design_type,
                "description" = trial@description,
                "trial_type" = trial@trial_type,
                "plot_width" = trial@plot_width,
                "plot_length" = trial@plot_length,
                "field_size" = trial@field_size,
                "planting_date" = trial@planting_date,
                "harvest_date" = trial@harvest_date,
                "plot_name" = plot@plot_name,
                "accession_name" = plot@accession_name,
                "plot_number" = plot@plot_number,
                "block_number" = plot@block_number,
                "is_a_control" = ifelse(plot@is_a_control, 1, NA),
                "rep_number" = plot@rep_number,
                "range_number" = plot@range_number,
                "row_number" = plot@row_number,
                "col_number" = plot@col_number,
                "seedlot_name" = plot@seedlot_name,
                "num_seed_per_plot" = plot@num_seed_per_plot,
                "weight_gram_seed_per_plot" = plot@weight_gram_seed_per_plot
            )

            # Add treatments, if provided
            for ( treatment in names(plot@treatments) ) {
                if ( plot@treatments[[treatment]] ) {
                    row[[treatment]] <- 1
                }
            }

            template <- dplyr::bind_rows(template, row)

        }

    }

    # Clean template
    if ( nrow(template) > 0 ) {
        for ( name in names(template) ) {
            template[name][which(template[name] == ""),] <- NA
        }
    }

    # Return the template
    return(template)

}


#' Write Trial Template
#' 
#' Write a breeDBase upload template (.xls file) for trials
#' 
#' @param input Either a vector of Trials to include in the template OR 
#' a \code{tibble} representation of the upload template
#' @param output The file path to the output .xls file
#' 
#' @family Trial
#' @import WriteXLS
#' @export
writeTrialTemplate <- function(
    input = NULL,
    output = NULL
) {

    # Check for required arguments
    if ( is.null(input) ) {
        stop("Cannot write Trial Template file: input of a template as a tibble or vector of trials is required")
    }
    if ( is.null(output) ) {
        stop("Cannot write Trial Template file: output of the file path to the .xls file is required")
    }

    # Create template if not provided one
    if ( !("tbl_df" %in% is(input)) ) {
        input <- buildTrialTemplate(input)
    }

    # Set output extension
    if ( !grepl("\\.xls$", output) ) {
        output <- paste(output, ".xls", sep="")
    }

    # Write the entire file
    else {
        print(sprintf("Writing Trial Template: %s", output))
        WriteXLS::WriteXLS(input, output)
    }

}

