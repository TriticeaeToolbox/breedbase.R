breeDBase.R
===========
### An R package for generating breeDBase upload templates

This R package (**currently a work in progress**) can be used to create 
classes of breeDBase data types (such as an Accession).  One or more 
instances of a class can be passed to a `buildTemplate` or `writeTemplate` 
function to create and/or write an upload template to be used for 
adding data through the breeDBase website.


## Example

Create new accessions and the upload template to add them to a breeDBase instance

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

calendonia <- Accession(
     "CALEDONIA", 
     "Triticum aestivum",
     list(
         synonyms = "PI610188",
         institute_codes = "CNL",
         organization_names = "Cornell University"
     )
)

accessions <- c(jerry, calendonia)


# Create the Upload Template
template <- buildAccessionTemplate(accessions)


# Write the Upload Template to a File
writeAccessionTemplate(template, '/path/to/file.xls')
```




## Current Status

The package currently contains functions for the following data types:

- Accession
- Location
