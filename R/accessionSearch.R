#' Get Accession Search Databases
#' 
#' Get all of the supported BrAPI-compliant databases configured by the accession search server.  
#' 
#' This function returns a vector of Accession Search Database objects, one of which can be used 
#' by the other accession search functions. The accession search server can be changed by setting 
#' the global \code{breedbase.accession_search_server} option (Example: 
#' \code{options("breedbase.accession_search_server" = "https://search.example.org")}).
#' 
#' @seealso \link{getAccessionSearchServer}
#' 
#' @return Vector of supported Accession Search Databases, where each Database is 
#' a list with the following names:
#' \describe{
#'   \item{name}{Database name}
#'   \item{address}{Database BrAPI URL}
#'   \item{version}{Database BrAPI version}
#'   \item{call_limit}{Max number of concurrent connections to the Databse}
#' }
#' 
#' @family accessionSearch
#' @export
getAccessionSearchDBs <- function() {
    dbs <- asGet("/databases")
    for ( i in c(1:length(dbs)) ) {
        db <- dbs[[i]]
        print(sprintf("%i = %s: %s [%s]", i, db$name, db$address, db$version))
    }
    return(dbs)
}

#' Get Accession Search Database
#' 
#' Get the Accession Search Database by the name of the BrAPI-compliant 
#' database configured by the accession search server.
#' 
#' The name must match the name of a database supported by the accession 
#' search server.  Use the \code{\link{getAccessionSearchDBs}} function 
#' to get a list of all of the supported databases.
#' 
#' @seealso \link{getAccessionSearchDBs}
#' 
#' @param name The name of the database
#' 
#' @return Accession Search Database, a list with the following names:
#' \describe{
#'   \item{name}{Database name}
#'   \item{address}{Database BrAPI URL}
#'   \item{version}{Database BrAPI version}
#'   \item{call_limit}{Max number of concurrent connections to the Databse}
#' }
#' 
#' @family accessionSearch
#' @export
getAccessionSearchDB <- function(name) {
    dbs <- asGet("/databases")
    for ( i in c(1:length(dbs)) ) {
        if ( tolower(name) == tolower(dbs[[i]]$name) ) {
            return(dbs[[i]])
        }
    }
    stop(sprintf("Database not found [%s]", name))
}

#' Create Accession Search Database
#' 
#' Create a custom Accession Search Database with the specified 
#' database properties
#' 
#' @param name Database Name
#' @param address Database BrAPI address
#' @param version Database BrAPI version
#' @param call_limit (optional) The maximum number of concurrent BrAPI calls
#' 
#' @return Accession Search Database, a list with the following names:
#' \describe{
#'   \item{name}{Database name}
#'   \item{address}{Database BrAPI URL}
#'   \item{version}{Database BrAPI version}
#'   \item{call_limit}{Max number of concurrent connections to the Databse}
#' }
#' 
#' @family accessionSearch
#' @export
createAccessionSearchDB <- function(name, address, version, call_limit=10) {
    return(list(
        name = name,
        address = address,
        version = version,
        call_limit = call_limit
    ))
}

#' Get Accession Search Cache Status
#' 
#' Get the status of the cached database terms for the provided Accession Search Database.
#' 
#' The cache status includes the timestamp of when the database terms were last loaded 
#' and the total number of database terms saved.  Use the \code{\link{updateAccessionSearchCache}} 
#' function to update the cached database terms for an Accession Search Database.
#' 
#' @seealso \link{updateAccessionSearchCache}
#' 
#' @param db Accession Search Database
#' 
#' @return Cache status information, a list with the following names:
#' \describe{
#'   \item{saved}{timestamp of when the cache was last saved}
#'   \item{terms}{the number of database terms saved}
#' }
#' 
#' @family accessionSearch
#' @export
getAccessionSearchCacheStatus <- function(db) {
    status <- asGet("/cache", list(address = db$address))
    return(status)
}

#' Update Accession Search Cache
#' 
#' Update the cache of database terms for the provided Accession Search Database.
#' 
#' @param db Accession Search Database
#' 
#' @return nothing
#' 
#' @family accessionSearch
#' @export 
updateAccessionSearchCache <- function(db) {
    invisible(asPut("/cache", body=db, results=FALSE))
}

#' Perform an Accession Search
#' 
#' Search the provided Accession Search Database to find database terms 
#' that match the provided search terms.
#' 
#' The accession search can be configured by providing a set of accession search 
#' configuration properties, which is a nested list with the following names:
#' 
#' | **NAME** | **DEFINITION** | **DEFAULT VALUE** |
#' | :-- | :---- | :--------- |
#' | **database_terms** | a list with the names: name, synonyms, accession_numbers | | 
#' | database_terms$name | flag to include accession names in the accession search | TRUE |
#' | database_terms$synonyms | flag to include accession synonyms in the accession search | TRUE |
#' | database_terms$accession_numbers | flag to include the accession numbers in the accession search | TRUE |
#' | **search_routines** | a list with the names: exact, punctuation, substring, prefix, edit_distance | |
#' | search_routines$exact | flag to use the exact name search routine in the accession search | TRUE |
#' | search_routines$punctuation | flag to use the punctuation search routine in the accession search | TRUE |
#' | search_routines$substring | flag to use the substring search routine in the accession search | TRUE |
#' | search_routines$prefix | flag to use the prefix search routine in the accession search | FALSE |
#' | search_routines$edit_distance | flag to use the edit distance search routine in the accession search | FALSE |
#' | **search_routine_options** | a list that has names of the supported search routines with values set as a list of the search routine options | |
#' | search_routine_options$substring$substring_length_min | the minimum length of a term to be included as a substring match | 3 |
#' | search_routine_options$prefix$prefixes | default prefixes to include | [] |
#' | search_routine_options$prefix$find_db_prefixes | scan the database terms to find common prefixes | TRUE |
#' | search_routine_options$prefix$prefix_length_min | when finding db prefixes, the minimum length of a prefix to include | 2 |
#' | search_routine_options$prefix$prefix_length_max | when finding db prefixes, the maximum length of a prefix to include | 5 |
#' | search_routine_options$prefix$threshold | when finding db prefixes, the minimum number of times a prefix is used before it is included | 250 |
#' | search_routine_options$edit_distance$max_edit_distance | the maximum number of changes for the edit distance comparison | 2 |
#' | **case_sensitive** | flag to perform a case-sensitive search | TRUE |
#' | **return_records** | flag to include the germplasm records with the search results | FALSE |
#' 
#' @param db Accession Search Database
#' @param terms Vector of accession search terms
#' @param config Accession Search Configuration
#' 
#' @return a \code{tibble} containing the search results. Example: 
#' 
#' | **search_term** | **search_routine** | **germplasm_name** | **germplasm_id** | **database_term** | **database_term_type** |
#' | :-------------- | :----------------- | :----------------- | :--------------- | :---------------- | :--------------------- |
#' | jerry | Exact Match | JERRY | 230227 | JERRY | name | 
#' | SY-Gold | Exact Match | 00X0100-51 | 232633 | Sy-Gold | synonym |
#' | PU0128A1_36 | Remove Punctuation | 0218A1-36 | 238801 | PU0128A1-36 | synonym |
#' 
#' @md 
#' @family accessionSearch
#' @export
performAccessionSearch <- function(db, terms, config=getBBOption("accession_search_config")) {
    
    # Make the POST request
    body <- list(
        database = db,
        terms = terms,
        config = config
    )
    results <- asPOST("/search", body=body, results=TRUE)
    
    # Format the results as a tibble
    t <- tibble::tibble(
        search_term = character(),
        search_routine = character(),
        germplasm_name = character(),
        germplasm_id = numeric(),
        database_term = character(),
        database_term_type = character()
    )
    for ( search_term in names(results) ) {
        matches <- results[[search_term]]$matches
        for ( germplasm_name in names(matches) ) {
            gm = matches[[germplasm_name]]
            for ( dm in gm$matched_db_terms ) {
                r <- tibble(
                    search_term = search_term,
                    search_routine = dm$search_routine$name,
                    germplasm_name = gm$germplasmName,
                    germplasm_id = gm$germplasmDbId,
                    database_term = dm$db_term$term,
                    database_term_type = dm$db_term$type
                )
                t <- dplyr::bind_rows(t, r)
            }
        }
    }

    # Return the tibble
    return(t)
}



## ======== ACCESSION SEARCH API FUNCTIONS ======== ##

# Make a GET request
# @param path Accession Search API Path
# @param [query] List of query parameters
# @return JSON body of the response
asGet <- function(path, query=NULL) {
    return(asHTTP("GET", path, query))
}

# Make a PUT request
# @param path Accession Search API Path
# @param [query] List of query parameters
# @param [body] List of body parameters
# @param [results] Flag to include the results if a job is spawned
# @return JSON body of the response
asPut <- function(path, query=NULL, body=NULL, results=FALSE) {
    return(asHTTP("PUT", path, query, body, results))
}

# Make a POST request
# @param path Accession Search API Path
# @param [query] List of query parameters
# @param [body] List of body parameters
# @param [results] Flag to include the results if a job is spawned
# @return JSON body of the response
asPOST <- function(path, query=NULL, body=NULL, results=FAlSE) {
    return(asHTTP("POST", path, query, body, results))
}

# Make an HTTP request
# @param method HTTP method name
# @param path Accession Search API Path
# @param [query] List of query parameters
# @param [body] List of body parameters
# @param [results] Flag to include the results if a job is spawned
# @return JSON body of the response
asHTTP <- function(method, path, query=NULL, body=NULL, results=FALSE) {
    url <- paste0(getBBOption("accession_search_server"), "/api", path)
    body <- tryCatch({
        ua <- paste0("breedbase.R/", utils::packageVersion("breedbase"), " (httr/", utils::packageVersion("httr"), ")")
        
        # Make the request with the appropriate httr function
        if ( toupper(method) == "GET" ) {
            resp <- httr::GET(
                url, 
                add_headers("Content-Type" = "application/json", "User-Agent" = ua), 
                query=query
            )
        }
        else if ( toupper(method) == "PUT" ) {
           resp <- httr::PUT(
                url, 
                add_headers("Content-Type" = "application/json", "User-Agent" = ua), 
                query=query,
                body=rjson::toJSON(body, results)
            )
        }
        else if ( toupper(method) == "POST" ) {
            resp <- httr::POST(
                url, 
                add_headers("Content-Type" = "application/json", "User-Agent" = ua), 
                query=query,
                body=rjson::toJSON(body, results)
            )
        }
        
        # Parse the body of the response as JSON
        resp_body <- httr::content(resp, "text", encoding="UTF-8")
        resp_body <- rjson::fromJSON(resp_body)
        resp_status <- resp_body$status

        # Parse the API status code
        if ( resp_status == 'success' ) {
            return(resp_body$response)
        }
        else if ( resp_status == 'queued' ) {
            resp_job <- asJob(resp_body$job$id, results)
            return(resp_job)
        }
        else if ( resp_status == 'running' ) {
            return(resp_body)
        }
        else if ( resp_status == 'complete' ) {
            return(resp_body)
        }
        else if ( resp_status == 'error' ) {
            stop(resp_body$error$message)
        }
        else {
            print(resp_body)
            stop(sprintf("Could not make API request [%s]", url))
        }
    }, error = function(e) {
        print(e)
        stop(sprintf("Could not make API request [%s]", url))
    })
}

# Process a job id: keep polling for its status and update its progress
# @param id Job id
# @param [results] Flag to include job results when complete
# @param [delay] Amount of time to wait until checking job status (seconds)
# @param [pb] Progress bar used for displaying job progress
# @return Job results when complete
asJob <- function(id, results=FALSE, delay=0, pb=txtProgressBar(min=0, max=100, style=3)) {
    Sys.sleep(delay)
    resp <- asGet(sprintf("/job/%s", id), list(results = ifelse(results, "true", "false")))
    if ( resp$status == 'complete' ) {
        setTxtProgressBar(pb, 100)
        close(pb)
        return(resp$job$results)
    }
    else if ( resp$status == 'running' ) {
        progress <- resp$job$progress
        setTxtProgressBar(pb, progress)
        delay <- delay+0.5
        if ( delay > 5 ) {
            delay <- 5
        }
        asJob(id, results, delay, pb)
    }
}