breeDBase.R
===========
### An R package for generating breeDBase upload templates

This R package (**currently a work in progress**) can be used to create 
classes of breeDBase data types (such as an Accession).  One or more 
instances of a class can be passed to a `buildTemplate` or `writeTemplate` 
function to create and/or write an upload template to be used for 
adding data through the breeDBase website.


## Installation

Currently, the breedbase package can be installed directly from GitHub using `devtools`.

```R
library(devtools)
install_github("TriticeaeToolbox/breeDBase.R")
```


## Documentation

Documentation for all Classes and functions can be found in the package's R documentation 
or online at [https://triticeaetoolbox.github.io/breeDBase.R/](https://triticeaetoolbox.github.io/breeDBase.R/).


## Example

Create new accessions, a cross pedigree, and the upload templates to add them to a breeDBase instance

```R
# Create the Accessions
jerry <- Accession(
     "JERRY", 
     "Triticum aestivum",
     list(
         synonyms = c("ND9257", "PI632433"),
         institute_codes = "NDSU",
         organization_names = "North Dakota State University"
     )
)

caledonia <- Accession(
     "CALEDONIA", 
     "Triticum aestivum",
     list(
         synonyms = "PI610188",
         institute_codes = "CNL",
         organization_names = "Cornell University"
     )
)

my_cross <- Accession(
    "MY_CROSS",
    "Triticum aestivum"
    list(
        institude_codes = "CNL",
        organization_names = "Cornell University"
    )
)

# Create a vector of accessions
accessions <- c(jerry, caledonia, my_cross)

# Set the pedigree for MY_CROSS
my_cross_pedigree <- Pedigree(my_cross, jerry, caledonia)


# Write the Accession Upload Template to a File
writeAccessionTemplate(accessions, '/path/to/accessions.xls')

# Write the Pedigree Upload Template to a File
writePedigreeTemplate(my_cross_pedigree, "/path/to/pedigrees.txt")
```


## Current Status

The package currently contains functions for the following data types:

- Accession
- Location
- Pedigree
