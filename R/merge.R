#' Merge Accessions
#' 
#' Create a merged accession template from multiple submissions
#' 
#' @param dir The path to the parent directory that contains the submission directories
#' @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
#' @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
#' 
#' @import readxl WriteXLS tibble dplyr readr digest
#' @export
mergeAccessions <- function(dir, useMostRecent=TRUE, keepUniqueDuplicates=FALSE) {
    mergeData(
        dir, 
        "accessions.xls", 
        buildAccessionTemplate(), 
        "accession_name", 
        writeAccessionTemplate, 
        useMostRecent,
        keepUniqueDuplicates
    )
}


#' Merge Locations
#' 
#' Create a merged location template from multiple submissions
#' 
#' @param dir The path to the parent directory that contains the submission directories
#' @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
#' @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
#' 
#' @import readxl WriteXLS tibble dplyr readr digest
#' @export
mergeLocations <- function(dir, useMostRecent=TRUE, keepUniqueDuplicates=FALSE) {
    mergeData(
        dir, 
        "locations.xls", 
        buildLocationTemplate(), 
        "Name", 
        writeLocationTemplate, 
        useMostRecent,
        keepUniqueDuplicates
    )
}


#' Merge Trials
#' 
#' Create a merged trial layout template from multiple submissions
#' 
#' @param dir The path to the parent directory that contains the submission directories
#' @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
#' @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
#' 
#' @import readxl WriteXLS tibble dplyr readr digest
#' @export
mergeTrials <- function(dir, useMostRecent=TRUE, keepUniqueDuplicates=FALSE) {
    mergeData(
        dir, 
        "trial_layout.xls", 
        buildTrialTemplate(), 
        "plot_name", 
        writeTrialTemplate, 
        useMostRecent,
        keepUniqueDuplicates
    )
}


#' Merge Plots
#' 
#' Create a merged plot data template from multiple submissions
#' 
#' @param dir The path to the parent directory that contains the submission directories
#' @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
#' @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
#' 
#' @import readxl WriteXLS tibble dplyr readr digest
#' @export
mergePlots <- function(dir, useMostRecent=TRUE, keepUniqueDuplicates=FALSE) {
    # mergeData(
    #     dir, 
    #     "trial_observations.xls", 
    #     buildPlotDataTemplate(), 
    #     "observationunit_name", 
    #     writePlotDataTemplate, 
    #     useMostRecent,
    #     keepUniqueDuplicates
    # )
    # PLOT DATA WILL HAVE TO BE HANDLED SEPARATELY:
    # Each file could have different columns
}


#' Merge All
#' 
#' Merge all of the supported data types
#' 
#' @param dir The path to the parent directory that contains the submission directories
#' @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
#' @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
#' 
#' @import readxl WriteXLS tibble dplyr readr digest
#' @export
mergeAll <- function(dir, useMostRecent=TRUE, keepUniqueDuplicates=FALSE) {
    mergeAccessions(dir, useMostRecent, keepUniqueDuplicates)
    mergeLocations(dir, useMostRecent, keepUniqueDuplicates)
    mergeTrials(dir, useMostRecent, keepUniqueDuplicates)
    mergePlots(dir, useMostRecent, keepUniqueDuplicates)
}


#' Merge Data
#' 
#' Perform the actual merging of the data with the provided properties
#' 
#' @param dir path to parent directory of the submissions
#' @param filename name of the file to merge
#' @param template blank template to hold the merged data
#' @param key the name of the key column for finding duplicates
#' @writeTemplate the write template function
#' @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
#' @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
mergeData <- function(dir, filename, template, key, writeTemplate, useMostRecent, keepUniqueDuplicates) {
    print(sprintf("Merging %s files from submissions in %s", filename, dir))

    # Get directories to parse
    dirs <- getSubmissionDirectories(dir, useMostRecent)

    # Merged table
    merged <- template

    # Parse each directory
    for ( sub_dir in dirs ) {
        a <- readxl::read_excel(paste(dir, sub_dir, filename, sep="/"))
        merged <- rbind(merged, a)
    }

    # Filter out duplicates
    filtered <- removeDuplicates(merged, key, keepUniqueDuplicates)

    # Write the filtered table
    dir.create(file.path(dir, "merged"), showWarnings=FALSE)
    writeTemplate(filtered, paste(dir, "merged", filename, sep="/"))
}



#' Remove Duplicates
#' 
#' Remove duplicate rows that have the same value in the specified key column
#' 
#' @param merged The merged tibble that contains all of the rows
#' @param key_column The name of the column that will be used to find unique values
#' @param keepUniqueDuplicates When TRUE, the script will not prompt to pick a unique duplicate row, but will keep all of them
#' 
#' @return a tibble with the duplicate rows removed
removeDuplicates <- function(merged, key_column, keepUniqueDuplicates) {
    
    # Get unique keys of the key column
    keys <- sort(unique(merged[[key_column]]))
    filtered <- readr::read_csv("\n", col_names=colnames(merged))
    
    # Parse each key
    for ( key in keys ) {
        
        # Subset the merged data for the key and get the number of rows
        s <- dplyr::filter(merged, !!as.name(key_column) == key)
        count <- nrow(s)

        # Key is only found once, add it to the filtered table
        if ( count == 1 ) {
            filtered <- rbind(filtered, s)
        }

        # Key is found more than once, compare the rows
        else {

            # list of unique rows (hash is the key, row is the value)
            unique_rows <- list()
            
            # Parse each row
            for ( i in c(1:count) ) {
                row <- s[i,]
                row_string <- ""
                
                # Create and store a hash of the row's values
                for ( j in colnames(row) ) {
                    row_string <- paste(row_string, row[[j]], sep="|")
                }
                hash <- digest::digest(row_string, algo="md5")
                unique_rows[[hash]] <- row
            }

            # There is only one unique row, add it to the filtered table
            if ( length(names(unique_rows)) == 1 ) {
                filtered <- rbind(filtered, unique_rows[[names(unique_rows)[[1]]]])
            }

            # There were multiple unique rows, the user needs to choose one or keep all
            else {
                
                # Display the unique rows and prompt user to pick one (or all) to keep
                if ( !keepUniqueDuplicates ) {
                    print(sprintf("WARNING: There were multiple rows for %s that are different!", key))
                    for ( i in c(1:length(names(unique_rows))) ) {
                        print(sprintf("ROW #%i", i))
                        print(unique_rows[[names(unique_rows)[[i]]]])
                    }
                    print("all: Keep all rows in the merged file")
                    keep <- readline(prompt="Enter row to keep (#/all): ")
                }

                # Keep all unique duplicates
                else {
                    keep <- "all"
                }
                
                # Keep all of the rows
                if ( keep == "all" ) {
                    for ( hash in names(unique_rows) ) {
                        filtered <- rbind(filtered, unique_rows[[hash]])
                    }
                }
                # Keep the selected row
                else {
                    filtered <- rbind(filtered, unique_rows[[names(unique_rows)[[as.integer(keep)]]]])
                }

            }
        }
    }

    # Return the filtered subset of the merged table
    return(filtered)

}



#' Get Submission Directories
#' 
#' Get the list of submission directories from the parent directory
#' 
#' @param dir The path to the parent directory that contains the submission directores
#' @param useMostRecent When set to TRUE, only include the most recent submission directory for each trial
#' 
#' @return vector of submission directories to parse
getSubmissionDirectories <- function(dir, useMostRecent=TRUE) {
    
    # Get the submission directories from the provided directory
    all_dirs <- list.files(dir, pattern="[0-9]{8}_[0-9]{6}_[0-9]+")
    rtn_dirs <- c()
    
    # Get the most recent submission of each trial
    if ( useMostRecent ) {
        trial_ts <- list()
        rtn_dirs_list <- list()
        for ( dir in all_dirs ) {
            parts <- strsplit(dir, "_")[[1]]
            trial <- parts[3]
            ts <- paste0(parts[1], parts[2])
            prev_ts <- trial_ts[[trial]]
            if ( is.null(prev_ts) || ts > prev_ts ) {
                trial_ts[[trial]] <- ts
                rtn_dirs_list[[trial]] <- dir
            }
        }
        for ( trial in names(rtn_dirs_list) ) {
            rtn_dirs <- c(rtn_dirs, rtn_dirs_list[[trial]])
        }
    }

    # Return all submissions, may include duplicate trials
    else {
        rtn_dirs <- all_dirs
    }

    # Return the filtered directories
    return(rtn_dirs)

}