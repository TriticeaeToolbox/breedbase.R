% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{getSupportedAccessionProperties}
\alias{getSupportedAccessionProperties}
\title{Get Supported Accession Properties}
\usage{
getSupportedAccessionProperties()
}
\value{
A vector of supported accession property names
}
\description{
Get a vector of accession property names that can be used by the Accession class.
These values can be overriden by setting the global \code{breedbase.editable_stock_props}
option (Example: \code{options("breedbase.editable_stock_props" = c("prop1", "prop2"))}).
}
\seealso{
Other Accession: 
\code{\link{Accession}},
\code{\link{Accession-class}},
\code{\link{Pedigree}},
\code{\link{Pedigree-class}},
\code{\link{buildAccessionTemplate}()},
\code{\link{buildPedigreeTemplate}()},
\code{\link{writeAccessionTemplate}()},
\code{\link{writePedigreeTemplate}()}
}
\concept{Accession}
