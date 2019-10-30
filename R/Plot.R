#' Plot Class
#' 
#' An S4 Class to represent a breeDBase phenotyping trial plot
#' 
#' The `plot_name`, `accession_name`, `plot_number`, and `block_number` 
#' are required, all other fields are optional
#' 
#' @slot plot_name The unique name for the plot (must be unique across entire database. this is often a concatenation of the trial name, the accession name, and the plot number.)
#' @slot accession_name The name of the accession being tested in the plot (must exist in the database)
#' @slot plot_number The sequential number for the plot in the field (e.g. 1001, 1002, 2001, 2002). these numbers should be unique for the trial.
#' @slot block_number A design parameter indicating which block the plot is in
#' @slot is_a_control (optional) TRUE if the plot is a control.  Generally, you will have accessions that are controls, so you should indicate the plots that that accession is in as a control.
#' @slot rep_number (optional) replicate number
#' @slot range_number (optional) range number, often synonymous with col_number
#' @slot row_number (optional) row number. if the field is a grid, this represents the y coordinate, required for field map generation. the top left plot should be row 1, column 1
#' @slot col_number (optional) column number. if the field is a grid, this represents the x coordinate. sometimes called range_number, required for field map generation. the top left plot shuold be row 1, column 1
#' @slot seedlot_name (optional) the seedlot from where the planted seed originated. must exist in the database
#' @slot num_seed_per_plot (optional) number seeds per plot. seed is transferred from seedlot mentioned in seedlot_name.
#' @slot weight_gram_seed_per_plot (optional) weight in gram of seeds in plot. seed is transferred from seedlot mentioned in seedlot name.
#' @slot treatments (optional) A named list where the name is the name of a treatment (e.g. inoculated, drought, etc) and the value should be TRUE if the treatment was applied to the plot or FALSE if not.
#' 
#' @importFrom methods is new slot<-
#' @export
setClass(
    "Plot",

    slots = list(
        plot_name = "character",
        accession_name = "character",
        plot_number = "numeric",
        block_number = "numeric",
        is_a_control = "logical",
        rep_number = "numeric",
        range_number = "numeric",
        row_number = "numeric",
        col_number = "numeric",
        seedlot_name = "character",
        num_seed_per_plot = "numeric",
        weight_gram_seed_per_plot = "numeric",
        treatments = "list"
    ),

    prototype = list(
        plot_name = NA_character_,
        accession_name = NA_character_,
        plot_number = NA_integer_,
        block_number = NA_integer_,
        is_a_control = FALSE,
        rep_number = NA_integer_,
        range_number = NA_integer_,
        row_number = NA_integer_,
        col_number = NA_integer_,
        seedlot_name = NA_character_,
        num_seed_per_plot = NA_real_,
        weight_gram_seed_per_plot = NA_real_,
        treatments = list()
    ),

    validity = function(object) {
        if ( is.na(object@plot_name) ) {
            return("Plot Name is required")
        }
        if ( is.na(object@accession_name) ) {
            return("Accession Name is required")
        }
        if ( is.na(object@plot_number) ) {
            return("Plot Number is required")
        }
        if ( is.na(object@block_number) ) {
            return("Block Number is required")
        }
        return(TRUE)
    }
)