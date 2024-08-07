% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotData.R
\docType{class}
\name{PlotData-class}
\alias{PlotData-class}
\title{PlotData Class}
\description{
An S4 Class to represent the phenotype data of a plot in a breeDBase phenotyping trial
}
\details{
The `plot_name` and `observations` parameters are required.
}
\section{Slots}{

\describe{
\item{\code{plot_name}}{The unique name for the plot (must already exist in the database)}

\item{\code{observations}}{A named list of phenotype observations trai-value key-pairs}

\item{\code{notes}}{(optional) Plot-level notes}
}}

\seealso{
Other Trial: 
\code{\link{Plot}},
\code{\link{Plot-class}},
\code{\link{PlotData}},
\code{\link{Trial}},
\code{\link{Trial-class}},
\code{\link{buildPlotDataTemplate}()},
\code{\link{buildPlotTemplate}()},
\code{\link{buildTrialTemplate}()},
\code{\link{createPlots}()},
\code{\link{getTrialDesignTypes}()},
\code{\link{getTrialTypes}()},
\code{\link{printPlots}()},
\code{\link{setTrialPlots}()},
\code{\link{writePlotDataTemplate}()},
\code{\link{writePlotTemplate}()},
\code{\link{writeTrialTemplate}()}
}
\concept{Trial}
