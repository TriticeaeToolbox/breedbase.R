breedbase.R
===========
### An R package for generating breedbase upload templates

This R package can be used to create classes of various breedbase data types (such as an Accession, a phenotyping Trial, a Plot of a phenotyping Trial, etc).  One or more instances of a class can be passed to a `buildTemplate` or `writeTemplate` function to create and/or write an upload template to be used for adding data through the breedbase website.


## Installation

Currently, the breedbase package can be installed directly from GitHub using `devtools`.

```R
# Install the devtools package, if you don't have it already
install.packages("devtools")

# Install the breedbase package directly from GitHub
library(devtools)
install_github("TriticeaeToolbox/breedbase.R")
library(breedbase)
```


## Documentation

Documentation for all Classes and functions can be found in the package's R documentation or online at [https://triticeaetoolbox.github.io/breedbase.R/reference](https://triticeaetoolbox.github.io/breedbase.R/reference).


## Examples


### Accessions and Pedigrees

#### Accessions

The `Accession` Class holds all of the information about a single Accession.  The `Accession()` function can be used to create an instance of the `Accession` Class.  The `accession_name` and `species_name` and required parameters.  Additional optional parameters can be provided through a named list passed as the `properties` parameter.  For a list of all of the supported optional parameters, see the [Accession Class Documentation](https://triticeaetoolbox.github.io/breedbase.R/reference/Accession-class.html) or use the `getSupportedAccessionProperties()` function to get a list of the names of all of the supported properties.

**NOTE:** With version 1.0.0 of the package, the Accession functions now use the new updated breedbase names for the Accession properties.  The old names are no longer supported by the package.

In order to avoid duplicate Accession entries in the database, it is recommended to perform a search of your Accession names against the names of those already in the database to find different possible variations in the spelling of the Accession name.  The package now includes functions that interface with the [BrAPI germplasm search tool](https://github.com/TriticeaeToolbox/BrAPI-germplasm-search) so Accession name searches can now be done directly in R.  See the [Accession Search](#accession-search) section below for more information on how to perform the search using this package.

**Example:** Create new Accessions with some optional properties

```R
jerry <- Accession(
     accession_name = "JERRY", 
     species_name = "Triticum aestivum",
     properties = list(
         synonym = c("ND9257", "PI632433"),
         "institute code" = "NDSU",
         organization_name = "North Dakota State University"
     )
)

caledonia <- Accession(
     accession_name = "CALEDONIA", 
     species_name = "Triticum aestivum",
     properties = list(
         synonym = "PI610188",
         "institute code" = "CNL",
         organization_name = "Cornell University"
     )
)

my_cross <- Accession(
    accession_name = "MY_CROSS",
    species_name = "Triticum aestivum",
    properties = list(
        "institute code" = "CNL",
        organization_name = "Cornell University"
    )
)

# Add all accessions to a vector
accessions <- c(jerry, caledonia, my_cross)
```

#### Pedigrees

The `Pedigree()` function can be used to create an object of the `Pedigree` Class which sets the parents (and  optionally the cross type) of a single Accession.

**Example:** Create a `Pedigree` object that sets the parents of the `my_cross` Accession as `jerry` and `caledonia`.

```R
my_cross_pedigree <- Pedigree(my_cross, female_parent = jerry, male_parent = caledonia)
```

**Note:** The **T3/Wheat** breedbase instance is planning on keeping pedigree information stored as an unparsed  purdy pedigree string (at least until we have time to try to parse purdy pedigrees into parents).  The purdy pedigree string will be stored as an optional property (`purdy_pedigree`) of the Accession and can be added to the Accession upload template.

#### Creating Upload Templates

Once you have one or more objects of a particular Class (such as a vector of `Accession`s or a `Pedigree`) you can  create the create the upload templates used to add that particular data type to a breedbase database through its website.

**Example:** Create an upload template for the 3 Accessions and 1 Pedigree.

```R
writeAccessionTemplate(accessions, '/path/to/accessions.xls')
writePedigreeTemplate(my_cross_pedigree, "/path/to/pedigrees.txt")
```


### Locations

Locations are required to have latitude and longitude coordinates as well as their altitude/elevation. These can be added manually as parameters or geocoded using the `geocodeLocation()` function which can lookup a street address or town name.  The function uses the [OpenStreetMap Nominatim API](https://nominatim.org/release-docs/develop/api/Overview/)  for geocoding and the [Global Multi-Resolution Topography PointServer Web Service](https://www.gmrt.org/services/pointserverinfo.php#!/services/getGMRTPoint) for elevation data.

Additionally, Locations are required to have a NOAA Station ID, which is used to get historical weather records.  The `lookupNOAAStationId()` function can be used to lookup a weather station close to the Location.

Use the `getCountryCodes()` function to get a list of the supported country codes and the `getLocationTypes()` function to get a list of the supported location types.

**Example:** Create a Location with all of the required parameters known
```R
loc1 <- Location(
    name = "Batavia, NY",
    abbreviation = "BAT",
    country_code = "USA",
    country_name = "United States of America",
    program = "Cornell",
    type = "Field",
    latitude = 42.98054,
    longitude = -78.23176,
    altitude = 274,
    noaa_station_id = "GHCND:US1NYGN0013"
)
```

**Example:** Create a Location without knowing its lat, lon or elevation/altitude.
```R
loc2 <- Location(
    name = "Batavia, NY",
    abbreviation = "BAT",
    country_code = "USA",
    country_name = "United States of America",
    program = "Cornell",
    type = "Field"
)
```
- If either the `latitude`, `longitude` or `altitude` parameters are not provided, they will be queried using the `name` as the location.
- If the `noaa_station_id` is not provided, it will be queried using the provided or queried lat and lon coordinates of the location.

**Example:** Geocode a specific address first, then use that information to create the Location
```R
geo <- geocodeLocation("2 Caldwell Drive, Ithaca, NY")
loc3 <- Location(
     name = "Ithaca, NY - Caldwell", 
     abbreviation = "ITHNY-CAL",
     country_code = "USA",
     country_name = "United States of America",
     program = "Cornell",
     type = "Field",
     latitude = geo$latitude,
     longitude = geo$longitude,
     altitude = geo$altitude
)
```

You can use the `lookupNOAAStationId()` function to get ID of the closest NOAA weather station to the specified geographic coordinates.
```R
stationId <- lookupNOAAStationId(geo$latitude, geo$longitude)
```


### Trials

A `Trial` includes the metadata about a single phenotyping trial as well as information about the individual plots in the trial (see the [Plots](#plots) section below). A trial is assigned a unique name, associated with a breeding program, year and location, and includes the trial design type and a description.  Additional information can include the field and plot sizes, planting and harvest dates, and type of trial (greenhouse, phenotyping, advanced yield, etc).

A `Trial` object can be created without its plot information (it just contains metadata about the trial).  However, the plot information must be added to the `Trial` before a trial upload template can be created.

**Example:** Create a Trial with just the required parameters

```R
trial_mad <- Trial(
    trial_name = "UMOPN_2019_Madison", 
    breeding_program = "University of Wisconsin", 
    location = "Madison, WI", 
    year = 2019, 
    design_type = "RCBD", 
    description = "UMOPN Nursery Trial"
)
```

**Example:** Create a Trial with optional parameters

```R
opts <- list(planting_date = "2019-04-25", harvest_date = "2019-10-05", trial_type = "phenotyping_trial")
trial_arl <- Trial(
    trial_name = "UMOPN_2019_Arlington", 
    breeding_program = "University of Wisconsin", 
    location = "Arlington, WI", 
    year = 2019, 
    design_type = "RCBD", 
    description = "UMOPN Nursery Trial", 
    properties = opts
) 
```

See the [Trial Class Documentation](https://triticeaetoolbox.github.io/breedbase.R/reference/Trial-class.html) for more information on the optional parameters.

Use the `getTrialDesignTypes()` function to get list of supported Trial design types and the `getTrialTypes()` function to get a list of supported Trial types.

**Adding Plots:** Once the `Plot`s for the trial have been created, they can be added to the `Trial` with the `setTrialPlots()` function.  See the [Plots section below](#plots) for more information on creating Trial Plots.

```R
trial_mad <- setTrialPlots(trial_mad, plots_mad)
trial_arl <- setTrialPlots(trial_arl, plots_arl)
```

**Creating Trial Templates:** Once you have one or more `Trial`s (that have `Plots` set), you can create a trial upload template that can be submitted to a breedbase website from the **Manage Trials** page ('Upload Existing Trials' > 'Multiple Trial Designs').

```R
writeTrialTemplate(c(trial_mad, trial_arl), '/path/to/trials.xls')
```


### Plots

A series of Plots are used to describe a phenotyping trial layout.  Each plot is assigned a unique name, a plot number, the name of the Accession used in the plot, and the block it is located in.  If the physical layout of the plots is known, each Plot can be assigned a row and column number.

 - A plot layout can be generated by manually creating each Plot with the desired parameters

```R
# Create the Plots
plot1 <- Plot(
    plot_name = "FARM-2019-UNH_PLOT1", 
    accession_name = "SL18-UCONN-S131", 
    plot_number = 1, 
    block_number = 1,
    properties = list(
        row_numer = 1,
        col_number = 1,
        rep_number = 1
    )
)
plot2 <- Plot(
    plot_name = "FARM-2019-UNH_PLOT2", 
    accession_name = "SL18-UCONN-S31", 
    plot_number = 2, 
    block_number = 1,
    properties = list(
        row_numer = 1,
        col_number = 2,
        rep_number = 1
    )
)

# Combine the Plots for the layout
plots <- c(plot1, plot2)

# Write the Plot layout template
writePlotLayout(plots, "/path/to/plots.xls")
```

- Alternatively, a simple plot layout can be generated automatically when given a list of Accessions and basic layout properties.

**Example**: A trial with 24 plots that has a maximum of 6 columns.  The blocks have a dimension of 3 columns by 2 rows.  The Plots will start in the top left corner and work across columns and down rows.
    
```R
# Generate the list of Accessions
# here we're using a dummy list of Accessions named ACC_A - ACC_X
accessions <- lapply(LETTERS[c(1:24)], function(x) { Accession(paste0("ACC_", x), "Saccharina latissima") })
   
# Create the Plots
plots <- createPlots(
    trial_name = "TEST-TRIAL", 
    accessions = accessions, 
    max_cols = 6, 
    max_cols_per_block = 3, 
    max_rows_per_block = 2
)
  
# The generated layout:
#                                     Col1                Col2                Col3                Col4                Col5                Col6
# ===== Row1 =====      ===== Plot 1 =====  ===== Plot 2 =====  ===== Plot 3 =====  ===== Plot 4 =====  ===== Plot 5 =====  ===== Plot 6 =====
# Row1: Plot Name         TEST-TRIAL_PLOT1    TEST-TRIAL_PLOT2    TEST-TRIAL_PLOT3    TEST-TRIAL_PLOT4    TEST-TRIAL_PLOT5    TEST-TRIAL_PLOT6
# Row1: Accession Name               ACC_A               ACC_B               ACC_C               ACC_D               ACC_E               ACC_F
# Row1: Block                            1                   1                   1                   2                   2                   2
# Row1: Control                      FALSE               FALSE               FALSE               FALSE               FALSE               FALSE
# ===== Row2 =====      ===== Plot 7 =====  ===== Plot 8 =====  ===== Plot 9 ===== ===== Plot 10 ===== ===== Plot 11 ===== ===== Plot 12 =====
# Row2: Plot Name         TEST-TRIAL_PLOT7    TEST-TRIAL_PLOT8    TEST-TRIAL_PLOT9   TEST-TRIAL_PLOT10   TEST-TRIAL_PLOT11   TEST-TRIAL_PLOT12
# Row2: Accession Name               ACC_G               ACC_H               ACC_I               ACC_J               ACC_K               ACC_L
# Row2: Block                            1                   1                   1                   2                   2                   2
# Row2: Control                      FALSE               FALSE               FALSE               FALSE               FALSE               FALSE
# ===== Row3 =====     ===== Plot 13 ===== ===== Plot 14 ===== ===== Plot 15 ===== ===== Plot 16 ===== ===== Plot 17 ===== ===== Plot 18 =====
# Row3: Plot Name        TEST-TRIAL_PLOT13   TEST-TRIAL_PLOT14   TEST-TRIAL_PLOT15   TEST-TRIAL_PLOT16   TEST-TRIAL_PLOT17   TEST-TRIAL_PLOT18
# Row3: Accession Name               ACC_M               ACC_N               ACC_O               ACC_P               ACC_Q               ACC_R
# Row3: Block                            3                   3                   3                   4                   4                   4
# Row3: Control                      FALSE               FALSE               FALSE               FALSE               FALSE               FALSE
# ===== Row4 =====     ===== Plot 19 ===== ===== Plot 20 ===== ===== Plot 21 ===== ===== Plot 22 ===== ===== Plot 23 ===== ===== Plot 24 =====
# Row4: Plot Name        TEST-TRIAL_PLOT19   TEST-TRIAL_PLOT20   TEST-TRIAL_PLOT21   TEST-TRIAL_PLOT22   TEST-TRIAL_PLOT23   TEST-TRIAL_PLOT24
# Row4: Accession Name               ACC_S               ACC_T               ACC_U               ACC_V               ACC_W               ACC_X
# Row4: Block                            3                   3                   3                   4                   4                   4
# Row4: Control                      FALSE               FALSE               FALSE               FALSE               FALSE               FALSE 
```

**Example:** The same 24 plots, but the plots are assigned using a zig-zag method (ie left to right in the first row, right to left in the second row, etc).  Additionally, the controls are specified using the names of the Accessions used as controls.

```R
# Generate the list of Accessions
accessions <- lapply(LETTERS[c(1:24)], function(x) { Accession(paste0("ACC_", x), "Saccharina latissima") })

# Create the Plots
plots <- createPlots(
    trial_name = "TEST-TRIAL", 
    accessions = accessions, 
    max_cols = 6, 
    max_cols_per_block = 3, 
    max_rows_per_block = 2,
    zig_zag = TRUE,
    controls = c("ACC_D", "ACC_L", "ACC_O", "ACC_T")
)

# The generated layout:
# ===== Row1 =====      ===== Plot 1 =====  ===== Plot 2 =====  ===== Plot 3 =====  ===== Plot 4 =====  ===== Plot 5 =====  ===== Plot 6 =====
# Row1: Plot Name         TEST-TRIAL_PLOT1    TEST-TRIAL_PLOT2    TEST-TRIAL_PLOT3    TEST-TRIAL_PLOT4    TEST-TRIAL_PLOT5    TEST-TRIAL_PLOT6
# Row1: Accession Name               ACC_A               ACC_B               ACC_C               ACC_D               ACC_E               ACC_F
# Row1: Block                            1                   1                   1                   2                   2                   2
# Row1: Control                      FALSE               FALSE               FALSE                TRUE               FALSE               FALSE
# ===== Row2 =====     ===== Plot 12 ===== ===== Plot 11 ===== ===== Plot 10 =====  ===== Plot 9 =====  ===== Plot 8 =====  ===== Plot 7 =====
# Row2: Plot Name        TEST-TRIAL_PLOT12   TEST-TRIAL_PLOT11   TEST-TRIAL_PLOT10    TEST-TRIAL_PLOT9    TEST-TRIAL_PLOT8    TEST-TRIAL_PLOT7
# Row2: Accession Name               ACC_L               ACC_K               ACC_J               ACC_I               ACC_H               ACC_G
# Row2: Block                            1                   1                   1                   2                   2                   2
# Row2: Control                       TRUE               FALSE               FALSE               FALSE               FALSE               FALSE
# ===== Row3 =====     ===== Plot 13 ===== ===== Plot 14 ===== ===== Plot 15 ===== ===== Plot 16 ===== ===== Plot 17 ===== ===== Plot 18 =====
# Row3: Plot Name        TEST-TRIAL_PLOT13   TEST-TRIAL_PLOT14   TEST-TRIAL_PLOT15   TEST-TRIAL_PLOT16   TEST-TRIAL_PLOT17   TEST-TRIAL_PLOT18
# Row3: Accession Name               ACC_M               ACC_N               ACC_O               ACC_P               ACC_Q               ACC_R
# Row3: Block                            3                   3                   3                   4                   4                   4
# Row3: Control                      FALSE               FALSE                TRUE               FALSE               FALSE               FALSE
# ===== Row4 =====     ===== Plot 24 ===== ===== Plot 23 ===== ===== Plot 22 ===== ===== Plot 21 ===== ===== Plot 20 ===== ===== Plot 19 =====
# Row4: Plot Name        TEST-TRIAL_PLOT24   TEST-TRIAL_PLOT23   TEST-TRIAL_PLOT22   TEST-TRIAL_PLOT21   TEST-TRIAL_PLOT20   TEST-TRIAL_PLOT19
# Row4: Accession Name               ACC_X               ACC_W               ACC_V               ACC_U               ACC_T               ACC_S
# Row4: Block                            3                   3                   3                   4                   4                   4
# Row4: Control                      FALSE               FALSE               FALSE               FALSE                TRUE               FALSE
```

**Creating Plot Templates:** Once you have a vector of `Plot`s for a single trial, a plot upload template can be created.  This upload template can be used to create a single Trial from the **Manage Trials** page ('Upload Existing Trial(s)' > 'Single Trial Design').  Here you will enter the Trial metadata in a form and upload this template for the trial's plot layout.

### Plot Data

The `PlotData` class is used to represent the phenotype observations (one or more traits) of a single Plot from a single Trial.  A vector of `PlotData` objects can be used to represent the observations from an entire Trial and can be used to create a plot data upload template that can be uploaded through a breedbase website.

A `PlotData` object is created with a `plot_name` and a named list of `observations` (and optionally `notes` for the plot).  

The `plot_name` must match the name of an existing Plot in an existing Trial.

The `observations` must take the form of a named list where the list item name/key is the full trait name of the trait observed and the list item value is the observed trait value.  The trait name must match an existing trait name in the breedbase database and follow the form {trait name}|{trait id} (ex: `Grain yield kg/ha|CO_321:0001218`).

**Example:** Create two `PlotData` objects representing two plots from the same Trial
```R
plotData1 <- PlotData(
    "FARM-2019-UNH_PLOT1", 
    list(
        "Blade length cm|CO_360:0000240" = 24, 
        "Blade width cm|CO_360:0000241" = 5
    )
)
plotData2 <- PlotData(
    "FARM-2019-UNH_PLOT2", 
    list(
        "Blade length cm|CO_360:0000240" = 27, 
        "Blade width cm|CO_360:0000241" = 4, 
        "Blade thickness mm|CO_360:0000248" = 2
    ), 
    notes="This plot showed signs of disturbance"
)
trialPlotData <- c(plotData1, plotData2)
```

**Creating Plot Data Templates:** Once you have a vector of `PlotData` objects for a single trial, a plot data upload template can be created using the `writePlotDataTemplate()` function.  This will create a a phenotyping results spreadsheet in the 'Simple' format with plot-level data. This file can be uploaded from the detail page of a specific Trial (under 'Upload Data Files' > 'Phenotyping Spreadsheets') or from the 'Manage Phenotypic Data' page.


## Conversion Functions

A number of helpful conversion functions are included to help convert phenotype observation values between  different commonly used units. All of these functions start with `convert_`, such as `convert_buac_kgHa`.  See the package Documentation for more details.

## Accession Search

The package now includes functions that can interface with a [BrAPI germplasm search](https://synonyms.triticeaetoolbox.org) server.  The search tool allows the user to find matching database entries that are the same as or similar to the user-provided search terms using a number of configurable search options.  Performing a search before submitting new Accessions to a breedbase database can limit the number of accidental duplicate entries in the database.

To perform a search, you first need to create an Accession Search Database object which contains the information about the breedbase instance you are going to search on.  The Accession search server has a number of pre-configured databases you can use (where the connection information is set by the search server) or you can create your own database specification.

To get and choose a pre-configured database from the search server:
```R
# Get a list of supported databases
dbs <- getAccessionSearchDBs()
[1] "1 = T3/Wheat: https://wheat.triticeaetoolbox.org/brapi/v1 [v1.3]"
[1] "2 = T3/Oat: https://oat.triticeaetoolbox.org/brapi/v1 [v1.3]"
[1] "3 = T3/Barley: https://barley.triticeaetoolbox.org/brapi/v1 [v1.3]"
[1] "4 = Cassavabase: https://cassavabase.org/brapi/v1 [v1.3]"
# Select a database to use
db <- dbs[[1]]
# Or get the Database by name
db <- getAccessionSearchDB("T3/Wheat")
```

To create your own database connection:
```R
db <- createAccessionSearchDB("T3/Wheat-Sandbox", "https://wheat-sandbox.triticeaetoolbox.org/brapi/v1", "v1.3")
```

Once you have a configured database connection, you can update the cache of database terms that the search server uses:
```R
updateAccessionSearchCache(db)
```

Then, you can perform a search on a vector of your search terms:
```R
search_terms <- c("jerry", "SY-Gold", "PU0128A1_36")
results <- performAccessionSearch(db, search_terms) 
```

The results are returned as a tibble with a column containing your search term and information about the matches:
```                                                                                                     
# A tibble: 3 x 6
  search_term search_routine     germplasm_name germplasm_id database_term database_term_type
  <chr>       <chr>              <chr>                 <dbl> <chr>         <chr>             
1 jerry       Exact Match        JERRY                230227 JERRY         name              
2 SY-Gold     Exact Match        00X0100-51           232633 Sy-Gold       synonym           
3 PU0128A1_36 Remove Punctuation 0128A1-36            238801 PU0128A1-36   synonym     
```


## Configurable Options

The breedbase package has a number of default property values that can be overriden using R's global options.  The following table lists the options that can be changed.  To set the value of an option, use the `options()` function with the name of the option and the new value (Example: to change the supported Accession properties: `options("breedbase.editable_stock_props" = c("variety", "class", "notes", "purdy_pedigree"))`

| Name | Definition&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; | Default Value |
| :--- | :--------- | :------------ |
| breedbase.standard_stock_props | Accession properties that are used by the breedbase website | `c("population_name", "organization_name", "synonym", "PUI")` |
| breedbase.editable_stock_props | Additional Accession properties defined by the individual breedbase instance | `c("variety", "released_variety_name", "donor", "donor institute", "donor PUI", "country of origin", "state", "institute code", "institute name", "biological status of accession code", "notes", "accession number", "seed source", "type of germplasm storage code", "acquisition date", "location_code", "ploidy_level", "genome_structure", "ncbi_taxonomy_id", "transgenic", "introgression_parent", "introgression_backcross_parent", "introgression_map_version", "introgression_chromosome", "introgression_start_position_bp", "introgression_end_position_bp", "purdy_pedigree", "filial_generation")` |
| breedbase.cross_types | The supported Pedigree cross types | `c("biparental", "self", "open", "sib")` |
| breedbase.country_codes | List of 3-letter country codes | `c('AFG','ALA','ALB','DZA','ASM','AND','AGO','AIA','ATA','ATG','ARG','ARM','ABW','AUS','AUT','AZE','BHS','BHR','BGD','BRB','BLR','BEL','BLZ','BEN','BMU','BTN','BOL','BES','BIH','BWA','BVT','BRA','IOT','VGB','BRN','BGR','BFA','BDI','CPV','KHM','CMR','CAN','CYM','CAF','TCD','CHL','CHN','HKG','MAC','CXR','CCK','COL','COM','COG','COK','CRI','CIV','HRV','CUB','CUW','CYP','CZE','PRK','COD','DNK','DJI','DMA','DOM','ECU','EGY','SLV','GNQ','ERI','EST','SWZ','ETH','FLK','FRO','FJI','FIN','FRA','GUF','PYF','ATF','GAB','GMB','GEO','DEU','GHA','GIB','GRC','GRL','GRD','GLP','GUM','GTM','GGY','GIN','GNB','GUY','HTI','HMD','VAT','HND','HUN','ISL','IND','IDN','IRN','IRQ','IRL','IMN','ISR','ITA','JAM','JPN','JEY','JOR','KAZ','KEN','KIR','KWT','KGZ','LAO','LVA','LBN','LSO','LBR','LBY','LIE','LTU','LUX','MDG','MWI','MYS','MDV','MLI','MLT','MHL','MTQ','MRT','MUS','MYT','MEX','FSM','MCO','MNG','MNE','MSR','MAR','MOZ','MMR','NAM','NRU','NPL','NLD','NCL','NZL','NIC','NER','NGA','NIU','NFK','MKD','MNP','NOR','OMN','PAK','PLW','PAN','PNG','PRY','PER','PHL','PCN','POL','PRT','PRI','QAT','KOR','MDA','REU','ROU','RUS','RWA','BLM','SHN','KNA','LCA','MAF','SPM','VCT','WSM','SMR','STP','SAU','SEN','SRB','SYC','SLE','SGP','SXM','SVK','SVN','SLB','SOM','ZAF','SGS','SSD','ESP','LKA','PSE','SDN','SUR','SJM','SWE','CHE','SYR','TJK','THA','TLS','TGO','TKL','TON','TTO','TUN','TUR','TKM','TCA','TUV','UGA','UKR','ARE','GBR','TZA','UMI','USA','VIR','URY','UZB','VUT','VEN','VNM','WLF','ESH','YEM','ZMB','ZWE')` |
| breedbase.location_types | The supported types of Locations | `c('Farm', 'Field', 'Greenhouse', 'Screenhouse', 'Lab', 'Storage', 'Other')` |
| breedbase.design_types | The supported Trial design types | `c('CRD', 'RCBD', 'Alpha', 'Augmented', 'MAD', 'Westcott', 'Lattice')` |
| breedbase.trial_types | The supported Trial types | `c('Seedling Nursery', 'phenotyping_trial', 'Advanced Yield Trial', 'Preliminary Yield Trial', 'Uniform Yield Trial', 'Variety Release Trial', 'Clonal Evaluation', 'genetic_gain_trial', 'storage_trial', 'heterosis_trial', 'health_status_trial', 'grafting_trial', 'Screen House', 'Seed Multiplication', 'crossing_block_trial', 'Specialty Trial')` |
| breedbase.accession_search_server | The URL of the [BrAPI germplasm search](https://github.com/TriticeaeToolbox/BrAPI-germplasm-search) server used by the Accession search functions  | `"https://synonyms.triticeaetoolbox.org"` |
| breedbase.accession_search_config | The default Accession search parameters | `list(database_terms = list(name = TRUE, synonyms = TRUE, accession_numbers = TRUE),search_routines = list(name = TRUE, punctuation = TRUE, substring = TRUE, edit_distance = FALSE, max_edit_distance = 2),return_records = FALSE)` |

## Current Status

The package currently contains functions for the following data types:

- Accession
- Pedigree
- Location
- Trial
- Plot
- PlotData
- Conversions
- Accession Searches (using the [BrAPI-germplasm-search tool](https://github.com/TriticeaeToolbox/BrAPI-germplasm-search)
