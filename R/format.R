# Headers in the location form that don't represent trait observations
LOCATION_FORM_NON_TRAIT_HEADERS = c("EXPT", "YEAR", "LOC", "TRIAL", "RANGE", "ROW", "COLUMN", "REP", "PLOT", "ENTRY", "ID", "DESIG", "PED")

# Values to be interpreted as NA
nas = c("", "-", "NA", ".", "N/A", "-9", "nd")


#
# TRAIT DEFINITIONS
# Define the mapping of trait codes used in the breeder data
# to T3 CO variables and conversion functions, if needed
#
TRAIT_DEFINITIONS = list(

	# Barley Traits
	"Hordeum vulgare" = list(
		"PLOT_YLD" = NULL,
		"YLD_BUPA" = list(
			trait = "Grain yield adjusted weight basis - kg/ha|CO_323:0000390",
			conversion = function(x, species) {
				cropWt = list("Hordeum vulgare" = 48, "Avena sativa" = 32, "Triticum aestivum" = 60)
				z = cropWt[[species]]
				if ( is.null(z) ) stop("Could not convert yield due to unknown crop weight conversion factor for species")
				y = x * z * 0.453592 * 2.47105
				return(y)
			}
		),
		"TW_LBBU" = NULL,
		"TKW" = "Thousand kernel weight - g|CO_323:0000572",
		"MD_JUL" = "Maturity time - Julian day|CO_323:0005515",
		"HD_JUL" = "Heading time - Julian day|CO_323:0005510",
		"HT_IN" = list(
			trait = "Plant height - cm|CO_323:0000494",
			conversion = function(x, species) {
				y = x * 2.54
				return(y)
			}
		),
		"LOD09" = "Lodging degree - 0-9 lodging degree scale|CO_323:0005514",
		"MOISTURE" = NULL,
		"PHE09" = NULL,
		"GRWHBT09" = NULL,
		"WNTDM09" = NULL,
		"WNTSUR" = NULL,
		"WNSUV%" = "Winter hardiness - %|CO_323:0005625",
		"KWEIGHT" = "Grain weight - mg|CO_323:0005506",
		"PMD09" = NULL,
		"LFRUST09" = NULL,
		"STRIPE09" = NULL,
		"STEMRUST09" = NULL,
		"CRUST09" = NULL,
		"HFLY09" = NULL,
		"BYDV09" = NULL,
		"FHB09" = NULL,
		"FHB09_MN" = NULL,
		"FHBSEV_MN" = "Fusarium Head Blight severity - %|CO_323:0005531",
		"FHBINC_MN%" = "Fusarium Head Blight incidence - %|CO_323:0005528",
		"FHBDON_MN" = "Grain Deoxynivalenol content - ppm|CO_323:0000344",
		"FHBDON_MEAN" = "Grain Deoxynivalenol content - ppm|CO_323:0000344",
		"FHBDON_RAW" = "Grain Deoxynivalenol content - ppm|CO_323:0000344",
		"FHBDON_STD" = NULL,
		"FHBFDK_MN%" = "Fusarium Damaged Kernels - %|CO_323:0005644",
		"FHBINX_MEAN" = "Fusarium Head Blight disease index - %|CO_323:0005643",
		"FHBINC/20HC_MEAN" = "Fusarium Head Blight incidence - 20HC - %|CO_323:0005646",
		"FHBINC_MEAN%" = "Fusarium Head Blight incidence - %|CO_323:0005528",
		"FHBPR09_MEAN" = "Fusarium Head Blight reaction type - 0-9 FHB Reaction Type Rating|CO_323:0005530",
		"FHBNIV_MN"	= "Grain nivalenol content - ppm|CO_323:0005645",
		"FHBNIV_MEAN" = "Grain nivalenol content - ppm|CO_323:0005645",
		"BLUES_DON" = NULL,
		"FALLNUM" = NULL,
		"PROTEIN" = NULL,
		"KHARDNESS" = NULL,
		"KDIAMETER" = NULL,
		"KWEIGHT" = NULL,
		"WATERABS" = NULL,
		"DEVTIME" = NULL,
		"STABILITY" = NULL,
		"GLUINDEX" = NULL,
		"WETGLU" = NULL,
		"DRYGLU" = NULL,
		"WBWG" = NULL,
		"OTHER" = "notes",
		"NOTES" = "notes"
	)

)


#'
#' Format Templates
#'
#' Convert templates in Jeanette Lyerly's format into T3/Breedbase upload templates
#'
#' @param input_file (required) path to input excel file containing the data to format
#' @param output_dir (required) path to output directory where the accessions, location, trials, and observations templates will be saved
#' @param breeding_program (required) name of breeding program to apply to all trials
#' @param species_name (required) name of species to apply to all accessions
#' @param design_type Trial Design Type applied to all trials (default: RCBD)
#' @param location_tab Name of tab that includes the location data (default: Location Data Form)
#' @param summary_tab Name of tab that includes the summary data (default: Summary Data Form)
#' @param data_descriptors_tab Name of tab that includes the trait codes and their definitions (default: Data_DESCRIPTORS)
#' @param trial_codes_tab Name of tab that includes the trial, year, and location codes (default: TRIAL_CODES)
#' @param location_data_start_row The index of the row that has the header columns for the location data (default: 3)
#' @param cooperator_start_row The index of the row that has the cooperator info in the summary tab (default: 1)
#' @param summary_data_start_row The index of the row that has the header columns for the summary data (default: 8)
#' @param data_descriptors_start_row The index of the row that has the first headers for the data descriptors tables (default: 1)
#' @param trial_codes_start_row The index of the row that has the headers for the trial codes table (default: 2)
#' @param trial_codes_start_col The index of the col that has the values for the trial codes table (default: C)
#' @param year_codes_start_col The index of the col that has the values for the year codes table (default: E)
#' @param location_codes_start_col The index of the col that has the values for the location codes table (default: J)
#' @param trait_definitions Additional trait definitions (this should be a list where the key is the name of the trait in the input data and the value is the trait header used in the breedbase template)
#' @param confirm_data Flag to print input data and have user confirm it is parsed correctly (default: TRUE)
#' @param write_templates Flag to have the function generate output templates (default: TRUE)
#' @param increment_year Flag to increment the year defined in the trial (used for winter crops where the input data is using the planting year as the year and we want to use the harvest year as the year) (default: FALSE)
#' @param standardize_trial_names Flag to generate standardized trial names in the format (PREFIX_YEAR_LOCATION) (Default: False, will use the Trial column in the input data)
#' @param trial_name_prefix When standardize_trial_names is set to True, this will be the prefix used in the generated trial names
#'
#' @import readxl tibble sjmisc
#' @export
formatTemplates <- function(input_file, output_dir, breeding_program, species_name,
			design_type = "RCBD",
			location_tab = "Location Data Form",
			summary_tab = "Summary Data Form",
			data_descriptors_tab = "Data_DESCRIPTORS",
			trial_codes_tab = "TRIAL_CODES",
			location_data_start_row = 3,
			cooperator_start_row = 1,
			summary_data_start_row = 8,
			data_descriptors_start_row = 1,
			trial_codes_start_row = 2,
			trial_codes_start_col = "C",
			year_codes_start_col = "E",
			location_codes_start_col = "J",
			trait_definitions = list(),
			confirm_data = TRUE,
			write_templates = TRUE,
			increment_year = FALSE,
			standardize_trial_names = FALSE,
			trial_name_prefix = NA
) {
	warnings = c()
	errors = c()
	addWarning = function(msg) {
		warnings <<- c(warnings, msg)
		message(paste0("WARNING: ", msg))
	}
	addError = function(msg) {
		errors <<- c(errors, msg)
		message(paste0("ERROR: ", msg))
	}

	# Set read ranges for the trial codes tab
	trial_codes_range = paste0(trial_codes_start_col, trial_codes_start_row, ":", LETTERS[which(LETTERS == trial_codes_start_col)+1], "999")
	year_codes_range = paste0(year_codes_start_col, trial_codes_start_row, ":", LETTERS[which(LETTERS == year_codes_start_col)+1], "999")
	location_codes_range = paste0(location_codes_start_col, trial_codes_start_row, ":", LETTERS[which(LETTERS == location_codes_start_col)+1], "999")

	# Read all of the input data sheets
	location_data = readxl::read_excel(input_file, sheet = location_tab, skip = location_data_start_row-1, na = nas, .name_repair = "unique_quiet")
	cooperator_data = readxl::read_excel(input_file, sheet = summary_tab, col_names = FALSE, skip = cooperator_start_row-1, n_max = summary_data_start_row-1, na = nas, .name_repair = "unique_quiet")
	summary_data = readxl::read_excel(input_file, sheet = summary_tab, skip = summary_data_start_row-1, na = nas, .name_repair = "unique_quiet")
	data_desciptors = readxl::read_excel(input_file, sheet = data_descriptors_tab, skip = data_descriptors_start_row-1, na = nas, .name_repair = "unique_quiet")
	trial_codes = readxl::read_excel(input_file, sheet = trial_codes_tab, range = trial_codes_range, na = nas, .name_repair = "unique_quiet")
	year_codes = readxl::read_excel(input_file, sheet = trial_codes_tab, range = year_codes_range, na = nas, .name_repair = "unique_quiet")
	location_codes = readxl::read_excel(input_file, sheet = trial_codes_tab, range = location_codes_range, na = nas, .name_repair = "unique_quiet")

	# Rotate the cooperator table
	cooperator_data = as_tibble(sjmisc::rotate_df(cooperator_data, cn = TRUE))

	# Ask user to check data
	if ( confirm_data == TRUE ) {

		# Print the parsed data
		print("==> LOCATION DATA:")
		print(location_data)
		print("==> COOPERATOR DATA:")
		print(cooperator_data)
		print("==> SUMMARY DATA:")
		print(summary_data)
		print("==> DATA DESCRIPTORS:")
		print(data_desciptors)
		print("==> TRIAL CODES:")
		print(trial_codes)
		print("==> YEAR CODES:")
		print(year_codes)
		print("==> LOCATION CODES:")
		print(location_codes)

		# Confirm data is OK
		a = readline("Check to make sure the input data was loaded correctly.\nContinue parsing the data? [Y/n] ")
		if ( tolower(a) != 'y' && a != '' ) {
			return()
		}

	}

	# Add additional user-provided trait definitions
	for ( td in names(trait_definitions) ) {
		TRAIT_DEFINITIONS[[species_name]][[td]] = trait_definitions[[td]]
	}

	#
	# LOCATIONS
	# - Gather all loc codes from the location and summary sheets
	# - Parse the town and state names for the used loc codes
	# - Set type to 'Town'
	# - Set country to USA
	# - Use provided breeding program
	# - Geocode lat and lon from town
	#
	locations = list()

	# Get all used location codes (from location and summary forms)
	used_location_codes = unique(c(na.omit(location_data$LOC), na.omit(summary_data$LOC)))

	# Parse each location code into a Location object
	print(sprintf("--> Building %i Locations...", length(used_location_codes)))
	for ( location_code in used_location_codes ) {
		
		# Find matching location in location codes
		m = filter(location_codes, CODE == location_code)
		if ( nrow(m) == 1 ) {
			
			# split location from ST-TOWN format into state and town parts
			location_parts = unlist(str_split(m$LOCATION, "-", 2))
			if ( length(location_parts) == 2 ) {
				state = location_parts[[1]]
				town = str_to_title(location_parts[[2]])

				# geocode location coordinates based on town
				coords = geocodeLocation(paste(town, state, sep=", "))

				# build location and add to list
				locations[[location_code]] = Location(
					name = paste(town, state, sep=", "),
					abbreviation = toupper(paste0(substring(town, 1, 3), state)),
					program = breeding_program,
					country_code = "USA",
					country_name = "United States of America",
					type = "Town",
					latitude = coords$latitude,
					longitude = coords$longitude,
					altitude = coords$altitude,
					noaa_station_id = ""
				)
			}

			# ERROR: location format incorrect
			else {
				addError(sprintf("The format of the location %s (code=%s) does not follow the ST-TOWN format", m$LOCATION, location_code))
			}
		}

		# ERROR: location not found in lookup
		else {
			addError(sprintf("The location code %s was not found in the %s worksheet", location_code, trial_codes_tab))
		}
	}


	#
	# ACCESSIONS
	# - Gather all used IDs from the location and summary sheets
	# - Use DESIG as synonym
	# - Use PED as purdy pedigree property
	#
	accessions = list()

	# Get all used sample IDs(from location and summary forms)
	sample_ids = unique(c(na.omit(location_data$ID), na.omit(summary_data$ID)))

	# Parse each sample ID
	print(sprintf("--> Building %i Accessions", length(sample_ids)))
	for ( id in sample_ids ) {

		# Get all instances for sample in location and summary data
		ml = filter(location_data, ID == id)
		ms = filter(summary_data, ID == id)

		# Get all unique DESIG and PED values for sample
		synonyms = unique(na.omit(ml$DESIG), na.omit(ms$DESIG))
		purdy_pedigrees = unique(na.omit(ml$PED), na.omit(ms$PED))
		female_parent = ""
		male_parent = ""
		cross_type = ""

		# parse simple A/B ped into parents
		if ( length(purdy_pedigrees) == 1 ) {
			purdy = purdy_pedigrees[[1]]
			if ( grepl("^[^/]+/[^/]+$", purdy) ) {
				purdy_parts = unlist(str_split(purdy, "/", 2))
				female_parent = purdy_parts[[1]]
				male_parent = purdy_parts[[2]]
				cross_type = "biparental"
			}
		}

		# build accession and add to list
		accessions[[id]] = Accession(
			accession_name = id,
			species_name = species_name,
			properties = list(
				synonym = synonyms,
				organization_name = breeding_program,
				purdy_pedigree = purdy_pedigrees,
				female_parent = female_parent,
				male_parent = male_parent,
				cross_type = cross_type
			)
		)
	}


	#
	# TRIAL LAYOUTS
	# - Use the data in the location data form to generate a trial for each location
	# - Build the plots before building the trial
	#
	trials = list()

	# Get all used trial IDs from the location form
	trial_ids = unique(na.omit(location_data$TRIAL))

	# Parse each trial
	print(sprintf("--> Building %i Trials...", length(trial_ids)))
	for ( trial_id in trial_ids ) {
		m = filter(location_data, TRIAL == trial_id)
		year_code = unique(m$YEAR)
		loc_code = unique(m$LOC)
		expt_code = unique(m$EXPT)

		# Ensure one year...
		if ( length(year_code) == 1 ) {
			year = filter(year_codes, CODE == year_code)$YEAR
			if ( increment_year ) {
				original_year = year
				year = year+1
				addWarning(sprintf("Year for trial %s changed: %i --> %i", trial_id, original_year, year))
			}

			# Ensure one location...
			if ( length(loc_code) == 1 ) {
				
				# Get location from code
				location = locations[[loc_code]]
				location_name = ifelse(!is.null(location), location@name, "UNKNOWN")
				location_town = gsub(",.*$", "", location_name)

				# Standardize trial name
				trial_name = trial_id
				if ( standardize_trial_names == TRUE && !is.na(trial_name_prefix) ) {
					trial_name = paste(trial_name_prefix, year, toupper(location_town), sep="_")
				}
				trial_name = gsub(" ", "_", trial_name)

				# Get additional cooperator properties
				mc = filter(cooperator_data, Location == loc_code)
				cooperator = NA
				planting_date = NA
				harvest_date = NA
				comments = NA
				if ( nrow(mc) == 1 ) {
					cooperator = mc$Cooperator
					planting_date = mc$Planted
					harvest_date = mc$Harvested
					comments = mc$Comments
				}

				#
				# PLOTS
				# - Generate plot name from trial id and plot number
				# - Set accession
				# - Set rep number (default = 1 if not provided)
				# - Set block number to rep number
				# - Set plot position based on row and col or range and row
				#
				plots = c()
				for ( i in c(1:nrow(m)) ) {
					row = m[i,]
					plot_number = row$PLOT
					accession_name = row$ID
					rep_number = row$REP
					range_number = row$RANGE
					row_number = row$ROW
					col_number = ifelse("COLUMN" %in% colnames(row), row$COLUMN, NA)
					entry_number = row$ENTRY

					# use row number for plot number, if not provided
					if ( is.na(plot_number) ) {
						plot_number = i;
						addWarning(sprintf("Trial %s, Row %i does not have a plot number set -- setting it to %i", trial_id, i, plot_number))
					}

					# generate plot name from trial and plot number
					plot_name = paste0(trial_name, "-PLOT_", plot_number)
					plot_name = gsub(" ", "_", plot_name)

					# set default rep number to 1
					if ( is.na(rep_number) ) {
						rep_number = 1
					}

					# use row and col, if provided
					# otherwise, use range and row
					plot_row = NA
					plot_col = NA
					if ( !is.na(row_number) && !is.na(col_number) ) {
						plot_row = row_number
						plot_col = col_number
					}
					else if ( !is.na(range_number) && !is.na(row_number) ) {
						plot_row = range_number
						plot_col = row_number
					}

					# set additional properties
					properties = list(
						rep_number = rep_number
					)

					# add plot positions, if set
					if ( !is.na(plot_row) && !is.na(plot_col) ) {
						properties$row_number = plot_row
						properties$col_number = plot_col
					}

					# add entry number, if set
					if ( !is.na(entry_number) ) {
						properties$entry_number = entry_number
					}

					plots = c(plots, Plot(
						plot_name = plot_name,
						accession_name = ifelse(!is.na(accession_name), accession_name, "UNKNOWN"),
						plot_number = plot_number,
						block_number = rep_number,
						properties = properties
					))
				}

				# Generate the description
				description = paste0(year, " ", expt_code, " trial at ", location_name)
				if ( !is.na(cooperator) ) {
					description = paste(description, "Cooperator:", cooperator, sep=" ")
				}
				if ( !is.na(comments) ) {
					description = paste(description, "Comments:", comments, sep=" ")
				}

				# Add additional trial properties
				properties = list(
					trial_type = "phenotyping_trial",
					plots = plots
				)

				# Add planting and harvest dates, if provided
				# convert excel dates to YYYY-MM-DD format
				if ( !is.na(planting_date) ) {
					if ( grepl("[0-9]{5}", planting_date) ) {
						planting_date = as.character(as.Date(as.numeric(planting_date), origin="1899-12-30"))
					}
					properties$planting_date = planting_date
				}
				if ( !is.na(harvest_date) ) {
					if ( grepl("[0-9]{5}", harvest_date) ) {
						harvest_date = as.character(as.Date(as.numeric(harvest_date), origin="1899-12-30"))
					}
					properties$harvest_date = harvest_date
				}

				# Generate the trial
				trial = Trial(
					trial_name = trial_name,
					breeding_program = breeding_program,
					location = location_name,
					year = year,
					design_type = design_type,
					description = description,
					properties = properties
				)

				# Add trial to list
				trials[[trial_id]] = trial
			}

			# ERROR: not only one location code
			else {
				addError(sprintf("There is not a single location code for trial %s", trial_id))
			}
		}

		# ERROR: not only one year code
		else {
			addError(sprintf("There is not a single year code for trial %s", trial_id))
		}
	}



	#
	# OBSERVATIONS
	#
	observations = list()
	undefined_traits = c()

	# Get all used trial IDs from the location form
	generated_trial_ids = names(trials)

	# Parse each trial
	print(sprintf("--> Building Observations for %i trials...", length(generated_trial_ids)))
	for ( trial_id in generated_trial_ids ) {
		print(sprintf("... Trial: %s", trial_id))
		m = filter(location_data, TRIAL == trial_id)

		# Remove empty columns
		# that way we're not bothering to parse traits that don't have any data
		m = Filter(function(x)!all(is.na(x)), m)

		# Parse each plot
		for ( i in c(1:nrow(m)) ) {
			row = m[i,]
			plot_name = trials[[trial_id]]@plots[[i]]@plot_name
			plot_data = list()
			
			# Parse the columns, if the plot name is defined
			if ( !is.na(plot_name) ) {
				
				# Get the trait values for each column
				for ( col in colnames(row) ) {
					value = as.character(row[[col]])

					# Remove R suffixes for duplicated column names (...#)
					col = gsub("\\.\\.\\.[0-9]+$", "", col)

					# Only process trait headers...
					# (those that are not defined as non-trait headers)
					if ( ! col %in% LOCATION_FORM_NON_TRAIT_HEADERS ) {

						# Lookup CO identifier
						co_trait = TRAIT_DEFINITIONS[[species_name]][[col]]
						if ( !is.null(co_trait) ) {
							if ( is.list(co_trait) ) {
								col = co_trait$trait
								value = suppressWarnings(as.numeric(value))
								value = co_trait$conversion(value, species_name)
							}
							else {
								col = co_trait
							}
						}
						else {
							undefined_traits = c(undefined_traits, col)
						}

						# Add value, if provided
						if ( !is.na(value) ) {
							plot_data[[col]] = value
						}
					}
				}

				# Add plot data to observations
				if ( length(plot_data) > 0 ) {
					observations[[plot_name]] = PlotData(plot_name, plot_data)
				}
				else {
					addWarning(sprintf("Plot %s (Trial %s) does not have any data", plot_name, trial_id))
				}

			}
			else {
				addError(sprintf("Plot in row %i of Trial %s does not have a plot name", i, trial_id))
			}
		}
	}

	# Warn for undefined traits
	if ( length(undefined_traits) > 0 ) {
		undefined_traits = sort(unique(undefined_traits))
		addWarning(sprintf("The following traits do not have a defined trait ontology term: %s", paste(undefined_traits, collapse=", ")))
	}



	#
	# WRITE TEMPLATES
	#

	if ( write_templates ) {
		print(sprintf("--> Writing templates to %s...", output_dir))
		if ( length(locations) > 0 ) {
			writeLocationTemplate(locations, paste(output_dir, "locations.xls", sep="/"))
		}
		if ( length(accessions) > 0 ) {
			writeAccessionTemplate(accessions, paste(output_dir, "accessions.xls", sep="/"), chunk=1000)
		}
		if ( length(trials) > 0 ) {
			writeTrialTemplate(trials, paste(output_dir, "trials.xls", sep="/"))
		}
		if ( length(observations) > 0 ) {
			writePlotDataTemplate(observations, paste(output_dir, "observations.xls", sep="/"))
		}
	}




	return(list(
		raw = list(
			location_data = location_data,
			cooperator_data = cooperator_data,
			summary_data = summary_data,
			data_desciptors = data_desciptors,
			trial_codes = trial_codes,
			year_codes = year_codes,
			location_codes = location_codes,
			trait_definitions = TRAIT_DEFINITIONS
		),
		parsed = list(
			locations = unlist(locations),
			accessions = unlist(accessions),
			trials = unlist(trials),
			observations = unlist(observations)
		),
		warnings = warnings,
		errors = errors
	))
}