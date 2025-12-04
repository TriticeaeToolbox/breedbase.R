#' Merge Accessions
#' 
#' Create a merged accession template from multiple submissions
#' 
#' @param dir The path to the parent directory that contains the submission directories (default=current directory)
#' @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
#' @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
#' 
#' @import readxl WriteXLS tibble dplyr readr digest
#' @export
mergeAccessions <- function(dir=".", useMostRecent=TRUE, keepUniqueDuplicates=FALSE) {
    mergeData(
        dir, 
        "accessions.xls", 
        "accession_name", 
        useMostRecent,
        keepUniqueDuplicates,
        chunk=2500
    )
}


#' Merge Locations
#' 
#' Create a merged location template from multiple submissions
#' 
#' @param dir The path to the parent directory that contains the submission directories (default=current directory)
#' @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
#' @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
#' 
#' @import readxl WriteXLS tibble dplyr readr digest
#' @export
mergeLocations <- function(dir=".", useMostRecent=TRUE, keepUniqueDuplicates=FALSE) {
    mergeData(
        dir, 
        "locations.xls", 
        "Name", 
        useMostRecent,
        keepUniqueDuplicates
    )
}


#' Merge Trials
#' 
#' Create a merged trial layout template from multiple submissions
#' 
#' @param dir The path to the parent directory that contains the submission directories (default=current directory)
#' @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
#' @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
#' 
#' @import readxl WriteXLS tibble dplyr readr digest
#' @export
mergeTrials <- function(dir=".", useMostRecent=TRUE, keepUniqueDuplicates=FALSE) {
    mergeData(
        dir, 
        "trial_layout.xls", 
        "plot_name", 
        useMostRecent,
        keepUniqueDuplicates,
        chunk=50,
        chunkBy="trial_name"
    )
}


#' Merge Plots
#' 
#' Create a merged plot data template from multiple submissions
#' 
#' @param dir The path to the parent directory that contains the submission directories (default=current directory)
#' @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
#' @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
#' 
#' @import readxl WriteXLS tibble dplyr readr digest
#' @export
mergePlots <- function(dir=".", useMostRecent=TRUE, keepUniqueDuplicates=FALSE) {
    mergeData(
        dir, 
        "trial_observations.xls",
        "observationUnitName",
        useMostRecent,
        keepUniqueDuplicates,
        include_cols = c("observationUnitName", "notes", "\\|CO_[0-9]+:[0-9]+$", "\\|COMP:[0-9]+$"),
        chunk=5000
    )
}


#' Merge All
#' 
#' Merge all of the supported data types
#' 
#' @param dir The path to the parent directory that contains the submission directories (default=current directory)
#' @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
#' @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
#' 
#' @import readxl WriteXLS tibble dplyr readr digest
#' @export
mergeAll <- function(dir=".", useMostRecent=TRUE, keepUniqueDuplicates=FALSE) {
    mergeAccessions(dir, useMostRecent, keepUniqueDuplicates)
    mergeLocations(dir, useMostRecent, keepUniqueDuplicates)
    mergeTrials(dir, useMostRecent, keepUniqueDuplicates)
    mergePlots(dir, useMostRecent, keepUniqueDuplicates)
}


# Merge Data
# 
# Perform the actual merging of the data with the provided properties
# 
# @param dir path to parent directory of the submissions
# @param filename name of the file to merge
# @param key the name of the key column for finding duplicates
# @param useMostRecent When set to TRUE, only use the most recent submission directory for each trial
# @param keepUniqueDuplicates When set to TRUE, the script will keep all unique versions of duplicate rows (and won't prompt the user to pick)
# @param skip_rows The number of rows to skip in the data file (before the header row)
# @param include_cols An array of colnames or regex statements of columns to include
# @param chunk Split data into multiple files of 'chunk' rows
# @param chunkBy When specified, chunk by the value in this column instead of by row
mergeData <- function(dir, filename, key, useMostRecent, keepUniqueDuplicates, skip_rows=0, include_cols=NULL, chunk=NULL, chunkBy=NULL) {
    print(sprintf("Merging %s files from submissions in %s", filename, dir))

    # Get directories to parse
    dirs <- getSubmissionDirectories(dir, useMostRecent)

    # Read the Excel files
    tables <- list()
    for ( sub_dir in dirs ) {
        table <- readxl::read_excel(paste(dir, sub_dir, filename, sep="/"), skip=skip_rows, col_types="text", na=c("NA"))
        if ( !is.null(include_cols) ) {
            keep_cols = c()
            for ( i in c(1:ncol(table)) ) {
                col = colnames(table)[i]
                for ( include in include_cols ) {
                    if ( col == include || grepl(include, col) ) {
                        keep_cols = c(keep_cols, i)
                    }
                }
            }
            table <- table[, keep_cols]
        }
        tables[[sub_dir]] <- table
    }

    # Get all of the unique column names
    cols <- c()
    for ( sub_dir in names(tables) ) {
        table <- tables[[sub_dir]]
        fc <- colnames(table)
        for ( c in fc ) {
            if ( ! c %in% cols ) {
                cols <- c(cols, c)
            }
        }
    }

    # Create an empty to template to merge into
    merged <- as_tibble(data.frame(matrix(ncol = length(cols), nrow = 0)))
    colnames(merged) <- cols
    for ( c in cols ) {
        merged[,c] <- as.character(merged[,c])
    }

    # Parse each table
    for ( sub_dir in names(tables) ) {
        table <- tables[[sub_dir]]
        merged <- dplyr::bind_rows(merged, table)
    }

    # Filter out duplicates
    filtered <- removeDuplicates(merged, key, keepUniqueDuplicates)

    # Write the filtered table
    dir.create(file.path(dir, "merged"), showWarnings=FALSE)
    output <- paste(dir, "merged", filename, sep="/")

    # Split the filtered data, if chunk is provided
    if ( !is.null(chunk) ) {

        max <- nrow(filtered)
        keys <- c()
        if ( !is.null(chunkBy) ) {
            keys <- unique(filtered[[chunkBy]])
            max <- length(keys)
        }
        index <- 1
        start <- 1
        end <- ifelse(max < chunk, max, chunk)
        while ( end <= max ) {

            # Subset the data, by this chunk of keys or rows
            subset <- tibble()
            if ( !is.null(chunkBy) ) {
                subset_keys <- keys[c(start:end)]
                subset <- filter(filtered, .data[[chunkBy]] %in% subset_keys)
            }
            else {
                subset <- filtered[c(start:end),]
            }

            # Write the subset output to the chunk file
            subset_output <- gsub("\\.xls$", paste0("_part", index, ".xls"), output)
            print(sprintf("...writing chunk %i: %s", index, subset_output))
            WriteXLS::WriteXLS(subset, subset_output)

            # Set next chunk
            if ( end == max ) {
                end <- max + 1
            }
            else {
                index <- index + 1
                start <- end + 1
                end <- end + chunk
                end <- ifelse(end > max, max, end)
            }

        }
    }

    # Write the entire file
    else {
        print(sprintf("...writing all data: %s", output))
        WriteXLS::WriteXLS(filtered, output)
    }
}



# Remove Duplicates
# 
# Remove duplicate rows that have the same value in the specified key column
# 
# @param merged The merged tibble that contains all of the rows
# @param key_column The name of the column that will be used to find unique values
# @param keepUniqueDuplicates When TRUE, the script will not prompt to pick a unique duplicate row, but will keep all of them
# 
# @return a tibble with the duplicate rows removed
removeDuplicates <- function(merged, key_column, keepUniqueDuplicates) {
    
    # Get unique keys of the key column
    keys <- sort(unique(merged[[key_column]]))
    filtered <- readr::read_csv("\n", col_names=colnames(merged), show_col_types=FALSE)
    
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
                print(sprintf("WARNING: There were multiple rows for %s that are different!", key))

                # Display the unique rows and prompt user to pick one (or all) to keep
                if ( !keepUniqueDuplicates ) {
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



# Get Submission Directories
# 
# Get the list of submission directories from the parent directory
# 
# @param dir The path to the parent directory that contains the submission directores
# @param useMostRecent When set to TRUE, only include the most recent submission directory for each trial
# 
# @return vector of submission directories to parse
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