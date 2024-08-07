% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locations.R
\name{lookupNOAAStationID}
\alias{lookupNOAAStationID}
\title{Lookup NOAA Station ID}
\usage{
lookupNOAAStationID(lat, lon, max_radius = 25)
}
\arguments{
\item{lat}{Location latitiude (decimal degrees)}

\item{lon}{Location longitude (decimal degrees)}

\item{radius}{Max Search radius (miles, default=25)}
}
\value{
The NOAA Station ID
}
\description{
Lookup the NOAA Station ID for the specified location.  The 
Location must be specified by its latitude and longitude 
(as decimal degrees) - if these are unknown the \code{\link{geocodeLocation}} 
function can be used to get the coordinates of a location by 
address, town, etc.
}
\details{
The query will search for NOAA stations within the specified max radius 
centered around the location position.  If multiple stations are found,
the ID of the first station is returned.
}
\seealso{
Other Location: 
\code{\link{Location}},
\code{\link{Location-class}},
\code{\link{buildLocationTemplate}()},
\code{\link{geocodeLocation}()},
\code{\link{getCountryCodes}()},
\code{\link{getLocationTypes}()},
\code{\link{writeLocationTemplate}()}
}
\concept{Location}
