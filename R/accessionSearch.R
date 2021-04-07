#' Get Accession Search Database Names
#' 
#' Get the names of all of the supported BrAPI-compliant databases configured 
#' by the accession search server
#' 
#' The \code{\link{getAccessionSearchDB}} function can be used to get the 
#' Accession Search Database needed for the search functions
#' 
#' @return Vector of supported database names
#' 
#' @export
getAccessionSearchDBs <- function() {
    dbs <- asGet("/databases")
    names <- c()
    for ( i in c(1:length(dbs)) ) {
        db <- dbs[[i]]
        print(sprintf("%s: %s [%s]", db$name, db$address, db$version))
        names <- c(names, db$name)
    }
    return(names)
}

#' Get Accession Search Database
#' 
#' Get the Accession Search Database by the name of the BrAPI-compliant 
#' database configured by the accession search server
#' 
#' @param name The name of the database
#' 
#' @return Accession Search Database
#' 
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
#' @return Accession Search Database
#' 
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
#' Get the status of the cached database terms for the 
#' provided Accession Search Database
#' 
#' @param db Accession Search Database
#' 
#' @return Cache status information, list(saved = timestamp of when cache was saved, terms = number of database terms saved)
#' 
#' @export
getAccessionSearchCacheStatus <- function(db) {
    status <- asGet("/cache", list(address = db$address))
    return(status)
}


updateAccessionSearchCache <- function(db) {
    resp <- asPut("/cache", body=db, results=FALSE)
    print("CACHE RESP")
    # print(resp)
}


performAccessionSearch <- function(db, terms, config=getBBOption("accession_search_config")) {
    body <- list(
        database = db,
        terms = terms,
        config = config
    )
    results <- asPOST("/search", body=body, results=TRUE)
    
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
        for ( match in matches ) {
            r <- tibble(
                search_term = search_term,
                search_routine = match$search_routine$name,
                germplasm_name = match$db_term$germplasmName,
                germplasm_id = match$db_term$germplasmDbId,
                database_term = match$db_term$term,
                database_term_type = match$db_term$type
            )
            t <- dplyr::bind_rows(t, r)
        }
    }

    return(t)
}

#
# SEARCH HELPER FUNCTIONS
#

asGet <- function(path, query=NULL) {
    return(asHTTP("GET", path, query))
}

asPut <- function(path, query=NULL, body=NULL, results=FALSE) {
    return(asHTTP("PUT", path, query, body, results))
}

asPOST <- function(path, query=NULL, body=NULL, results=FAlSE) {
    return(asHTTP("POST", path, query, body, results))
}

asHTTP <- function(method, path, query=NULL, body=NULL, results=FALSE) {
    url <- paste0(getBBOption("accession_search_api"), path)
    body <- tryCatch({
        ua <- paste0("breedbase.R/", utils::packageVersion("breedbase"), " (httr/", utils::packageVersion("httr"), ")")
        
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
        
        resp_body <- httr::content(resp, "text", encoding="UTF-8")
        resp_body <- rjson::fromJSON(resp_body)
        resp_status <- resp_body$status

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