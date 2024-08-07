% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locations.R
\name{geocodeLocation}
\alias{geocodeLocation}
\title{Geocode Location}
\usage{
geocodeLocation(location)
}
\arguments{
\item{location}{Location to parse (ie Ithaca, NY; Caldwell Dr, Ithaca, NY; etc...)}
}
\value{
A list with components:
\describe{
  \item{latitude}{Location latitude (decimal degrees)}
  \item{longitude}{Location longitude (decimal degrees)}
  \item{altitude}{Location elevation (meters)}
}
}
\description{
Lookup the coordinates and elevation for the specified 
location.  The location text will be parsed by DataScienceToolkit's
`street2coordinates` API into coordinates and the elevation will be 
queried using the `coordinates2statistics` API.
}
\examples{
caldwell <- geocodeLocation("2 Caldwell Dr, Ithaca, NY")
lat <- caldwell$latitude
lon <- caldwell$longitude
alt <- caldwell$altitude

}
\seealso{
Other Location: 
\code{\link{Location}},
\code{\link{Location-class}},
\code{\link{buildLocationTemplate}()},
\code{\link{getCountryCodes}()},
\code{\link{getLocationTypes}()},
\code{\link{lookupNOAAStationID}()},
\code{\link{writeLocationTemplate}()}
}
\concept{Location}
