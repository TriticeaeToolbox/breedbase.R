% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locations.R
\name{buildLocationTemplate}
\alias{buildLocationTemplate}
\title{Build Location Template}
\usage{
buildLocationTemplate(locations = NULL)
}
\arguments{
\item{locations}{Vector of Locations to add to the template}
}
\value{
A \code{tibble} representation of the upload template
}
\description{
Create a \code{tibble} representing the breeDBase upload 
template for the provided locations
}
\seealso{
Other Location: 
\code{\link{Location}},
\code{\link{Location-class}},
\code{\link{geocodeLocation}()},
\code{\link{getCountryCodes}()},
\code{\link{getLocationTypes}()},
\code{\link{lookupNOAAStationID}()},
\code{\link{writeLocationTemplate}()}
}
\concept{Location}
