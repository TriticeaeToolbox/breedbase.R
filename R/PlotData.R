#' PlotData Class
#' 
#' An S4 Class to represent the phenotype data of a plot in a breeDBase phenotyping trial
#' 
#' The `plot_name` and `observations` parameters are required.
#' 
#' @slot plot_name The unique name for the plot (must already exist in the database)
#' @slot observations A named list of phenotype observations trai-value key-pairs
#' @slot notes (optional) Plot-level notes
#' 
#' @importFrom methods is new slot<-
#' @export
setClass(
    "PlotData",

    slots = list(
        plot_name = "character",
        observations = "list",
        notes = "character"
    ),

    prototype = list(
        plot_name = NA_character_,
        observations = list(),
        notes = NA_character_
    ),

    validity = function(object) {
        if ( is.na(object@plot_name) ) {
            return("Plot Name is required")
        }
        if ( length(object@observations) == 0 ) {
            return("Observations are required")
        }
        return(TRUE)
    }
)