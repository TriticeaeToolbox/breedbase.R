% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessions.R
\name{Accession}
\alias{Accession}
\title{Create Accession}
\usage{
Accession(accession_name = NULL, species_name = NULL, properties = list())
}
\arguments{
\item{accession_name}{Accession Name}

\item{species_name}{Species Name}

\item{properties}{(optional) Additional accession properties (as a named list)

See function \code{\link{getSupportedAccessionProperties}} for all supported accession property names}
}
\value{
Accession
}
\description{
Create an Accession containing all of the provided properties. 
Accession Name and Species Name are required.  All optional 
properties can be added as a named value in the properties list.
}
\examples{
accession <- Accession(
     "JERRY", 
     "Triticum aestivum",
     list(
         synonym = c("ND9257", "PI632433"),
         "institute code" = "NDSU",
         organization_name = "North Dakota State University"
     )
)

}
\seealso{
\link{getSupportedAccessionProperties}

Other Accession: 
\code{\link{Accession-class}},
\code{\link{Pedigree}},
\code{\link{Pedigree-class}},
\code{\link{buildAccessionTemplate}()},
\code{\link{buildPedigreeTemplate}()},
\code{\link{getSupportedAccessionProperties}()},
\code{\link{writeAccessionTemplate}()},
\code{\link{writePedigreeTemplate}()}
}
\concept{Accession}
