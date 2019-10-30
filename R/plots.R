#' Create Plot
#' 
#' Create a phenotyping trial Plot containing all of the provided 
#' properties.  Plot Name, Accession Name, Plot Number and Block Number 
#' are required.  All optional properties can be added as a named value 
#' in the properties list.
#' 
#' @param plot_name The unique name for the plot (must be unique across entire database. this is often a concatenation of the trial name, the accession name, and the plot number.)
#' @param accession_name The name of the accession being tested in the plot (must exist in the database)
#' @param plot_number The sequential number for the plot in the field (e.g. 1001, 1002, 2001, 2002). these numbers should be unique for the trial.
#' @param block_number A design parameter indicating which block the plot is in
#' @param properties (optional) Additional plot properties (as a named list)
#' 
#' See Class \linkS4class{Plot} for all optional plot properties
#' 
#' @examples
#' # Create plots with just the required parameters
#' plot1 <- Plot("FARM-2019-UNH_PLOT1", "SL18-UCONN-S131", 1, 1)
#' plot2 <- Plot("FARM-2019-UNH_PLOT2", "SL18-UCONN-S31", 2, 1)
#' 
#' # Create plots with optional parameters
#' plot3 <- Plot("FARM-2019-UNH_PLOT3", "SL18-UCONN-S105", 3, 1, list(row_number = 1, col_number = 3))
#' plot4 <- Plot("FARM-2019-UNH_PLOT4", "SL18-UCONN-S146", 4, 1, list(row_number = 1, col_number = 4))
#' 
#' # Create plots with treatments
#' plot5 <- Plot(
#'      "FARM-2019-UNH_PLOT5", 
#'      "SL18-UCONN-S110", 
#'      5, 
#'      1, 
#'      list(
#'          row_number = 1, 
#'          col_number = 5, 
#'          treatments = list(
#'              inoculated = TRUE
#'          )
#'      )
#' )
#' 
#' @return Plot
#' 
#' @export
Plot <- function(
    plot_name = NULL,
    accession_name = NULL,
    plot_number = NULL,
    block_number = NULL,
    properties = list()
) {

    # Check for required properties
    if ( is.null(plot_name) ) {
        stop("Cannot create Plot: plot name required")
    }
    if ( is.null(accession_name) ) {
        stop("Cannot create Plot: accession name required")
    }
    if ( is.null(plot_number) ) {
        stop("Cannot create Plot: plot number required")
    }
    if ( is.null(block_number) ) {
        stop("Cannot create Plot: block number required")
    }

    # Create Plot with required properties
    plot <- new(
        "Plot",
        plot_name = plot_name,
        accession_name = accession_name,
        plot_number = plot_number,
        block_number = block_number
    )

    # Add optional properties
    for ( name in names(properties) ) {
        value <- unname(properties[name])[[1]]
        slot(plot, name) <- value
    }

    # Return the Plot
    return(plot)

}


#' Create Plots
#' 
#' Create a set of plots from a list of ordered Accessions and basic layout properties.
#' The plots will start in the top left corner and move across rows and down columns.  Blocks
#' will increment in the same order as plots.  If `zig_zag` is set to TRUE then rows will 
#' alternate the direction of plot numbers.
#' 
#' @param trial_name The trial name to be used for plot names ({trial_name}_PLOT#)
#' @param accessions List of Accessions or Accession names in plot order (moving across rows)
#' @param max_cols The maximum number of columns (plots) in a row
#' @param max_cols_per_block (optional, default = no max) The maximum number of columns in a block
#' @param max_rows_per_block (optional, default = no max) The maximum number of rows in a block
#' @param zig_zag (optional, default = FALSE) When TRUE, rows will alternate direction (left to right, right to left, etc)
#' @param controls (optional, default = none) A vector of plot numbers or accession names that will be used as controls
#' 
#' @examples
#' # When given a list of 18 accessions (ACC_A -> ACC_R), a max_cols of `6` 
#' # and a max_cols_per_block of `3`, the following plots will be generated:
#' #     
#' #         COL 1   COL 2   COL 3   COL 4   COL 5   COL 6
#' # ROW 1   #1      #2      #3      #4      #5      #6
#' #         ACC_A   ACC_B   ACC_C   ACC_D   ACC_E   ACC_F
#' #         BLK 1   BLK 1   BLK 1   BLK 2   BLK 2   BLK 2
#' # 
#' # ROW 2   #7      #8      #9      #10     #11     #12
#' #         ACC_G   ACC_H   ACC_I   ACC_J   ACC_K   ACC_L
#' #         BLK 1   BLK 1   BLK 1   BLK 2   BLK 2   BLK 2
#' # 
#' # ROW 3   #13     #14     #15     #16     #17     #18
#' #         ACC_M   ACC_N   ACC_O   ACC_P   ACC_Q   ACC_R
#' #         BLK 1   BLK 1   BLK 1   BLK 2   BLK 2   BLK 2
#' accessions <- lapply(LETTERS[c(1:18)], function(x) {Accession(paste0("ACC_", x), "Saccharina latissima")})
#' plots <- createPlots(accessions, 6, 3)
#' 
#' @return vector of Plots
#' 
#' @export
createPlots <- function(
    trial_name = NULL,
    accessions = NULL,
    max_cols = NULL,
    max_cols_per_block = NULL,
    max_rows_per_block = NULL,
    zig_zag = FALSE,
    controls = c()
) {

    # Check required arguments
    if ( is.null(trial_name) ) {
        stop("Cannot create Plots: trial name required")
    }
    if ( is.null(accessions) ) {
        stop("Cannot create Plots: accessions required")
    }
    if ( is.null(max_cols) ) {
        stop("Cannot create Plots: max columns required")
    }

    # Calculate number of rows
    count <- length(accessions)
    max_rows <- ceiling(count/max_cols)
    print(sprintf("Creating plots in %i columns and %i rows", max_cols, max_rows))

    # Vector of plots to return
    plots <- c()
    plot <- 1
    block <- 1

    # Parse each row and columns
    for ( row in c(1:max_rows) ) {

        # Set blocks for the current row
        # TODO

        for ( col in c(1:max_cols) ) {
            
            # Setup plot
            if ( plot <= count ) {
                print(sprintf("...Plot %i: Row %i, Col %i", plot, row, col))

                # Set plot and accession names
                plot_name <- paste0(trial_name, "_", "PLOT", plot)
                accession_name <- accessions[[plot]]
                if ( is(accession_name, "Accession") ) {
                    accession_name <- accession_name@accession_name
                }

                # Create the plot
                plots <- c(plots, Plot(
                    plot_name = plot_name,
                    accession_name = accession_name,
                    plot_number = plot,
                    block_number = block,
                    properties = list(
                        row_number = row,
                        col_number = col
                    )
                ))
            }

            # Increment plot number
            plot <- plot + 1

        }
    }

    # Return the plots
    return(plots)

}


#' Print Plots
#' 
#' Create a tibble mirroring the plot layout of the provided plots 
#' that can be printed to the console to inspect the plot layout.
#' 
#' @param plots The Plots to print
#' 
#' @import tibble
#' @export
printPlots <- function(plots) {

    # Get the layout dimensions
    max_cols <- 1
    max_rows <- 1
    for ( plot in plots ) {
        if ( plot@col_number > max_cols ) {
            max_cols <- plot@col_number
        }
        if ( plot@row_number > max_rows ) {
            max_rows <- plot@row_number
        }
    }

    # Tibble of plot layout to display
    rtn <- tibble()

    # Add Columns
    for ( i in c(1:max_cols) ) {
        rtn <- add_column(rtn, !!(paste0("Col", i)) := NA)
    }

    # Add Rows
    row_names <- c()
    headers <- c("Plot #", "Plot Name", "Accession Name", "Block")
    for ( i in c(1:max_rows) ) {
        for ( j in c(1:length(headers)) ) {
            header <- headers[j]
            rtn <- add_row(rtn)
            row_names <- c(row_names, paste0("Row", i, ": ", header))
        }
    }
    
    # Set Row Names
    rtn$row_names <- row_names
    rtn <- remove_rownames(rtn)
    rtn <- column_to_rownames(rtn, var = "row_names")

    # Plot Index
    index <- 1

    # Parse each plot
    for ( plot in plots ) {
        plot_row <- plot@row_number
        plot_col <- plot@col_number
        block <- plot@block_number
        plot_number <- plot@plot_number
        plot_name <- plot@plot_name
        accession_name <- plot@accession_name

        # Calculate starting table row
        table_row_start <- (plot_row * length(headers)) - (length(headers) - 1)

        # Add Values to Table
        rtn[table_row_start, plot_col] <- paste0("==== Plot ", plot_number, " ====")
        rtn[table_row_start+1, plot_col] <- plot_name
        rtn[table_row_start+2, plot_col] <- accession_name
        rtn[table_row_start+3, plot_col] <- block
    }

    # Return the tibble
    return(rtn)

}
