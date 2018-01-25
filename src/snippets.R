# TODO: Review function to review validity of entry
# TODO: FUnction to change single values in table
# TODO: Delete Functions
# TODO: Functions to find duplicates


# Two more generic functions for matching matching fuzzy duplicates

# check for duplicates always in create functions, unless checkduplicat
# Functions for removal from tables with id

# For duplicates: get column names of table, then for each column perform fuzzy
# matching, maybe with stringdistmatrix, give one result table. Maybe generic
# duplicates function, that can be called per field
# Also FindDuplicateAssessors, etc.

if(internal_matching == TRUE){
  matched_ids <- c(matched_ids, matched_ids_internal)
  matched_rows <- c(matched_rows, matched_rows_internal)
}



studies <- studies[c(1:5, 5:1),]
fields  <-  c("abbreviation", "authors")
fuzzy.min.sims <- c(1, 1)
table="studies"
id.field="study_id"
all.entries <- T
source=studies

CheckForDuplicates(source=source, table="studies", fields=c("abbreviation", "authors"),
                   id.field="study_id", fuzzy.min.sims=c(0.95, 0.1), all.entries=F, ids.only = F)

CheckForDuplicates(source=NULL, table="studies", fields=c("abbreviation"),
                   id.field="study_id", fuzzy.min.sims=c(0.5), all.entries=F, ids.only=F)

GetStudies()






source=studies
fields= c("abbreviation", "authors", "title")
fuzzy.min.sims=c(0.9, 0.9, 0.8)
all.entries=F 
ids.only=F 
conn=eaDB

part <- studies[6,]
fields="title"
fuzzy.min.sims=0.8
source  <-  part

CheckForDuplicateStudies(studies)
CheckForDuplicateAssessors(assessors)

studies
GetStudies()[c(6,12),]





# Create template for duplicate matching
examples <- read.csv("../data/example_studies.csv")
studies <- examples[,1:5]
studies <- studies[c(5,2,1,12,9),]

############################ ##
# Old Functions            ####
############################ ##


GetAssessors_OLD <- function(select = NULL, field = "name"){
  # Gets a list of assessors and functions as a wrapper for GetRecords
  #
  # Args
  #   select: Search term for defined field. If NULL the entire assessors table
  #       is returned. Default is NULL.
  #   field: Field to be queried; either "name" or "email". Default is "email".
  #
  # Returns
  #   The entire assessors table or only rows matching the search term.
  if(is.null(select)){
    assessors <- dbGetQuery(con, "SELECT * FROM assessors;")
  } else {
    if(field == "name"){
      select <- paste("%", as.character(select), "%", sep = "")
      assessors <- dbGetQuery(con, "SELECT * FROM assessors WHERE name LIKE ?;", 
                              param=list(select))
    } else {
      select <- paste("%", as.character(select), "%", sep = "")
      assessors <- dbGetQuery(con, "SELECT * FROM assessors WHERE email LIKE ?;", 
                              param=list(select))
    }
  }
  return(assessors) 
}

new_studies <- function(studies, silent = FALSE){
  # studies: either a named list (for one entry) or a data.frame (for several
  # entries) with mandatory fields "abbreviation ", "authors", "title", and
  # "year"; and optional field "doi"
  
  studies <- data.frame(lapply(studies, as.character), stringsAsFactors = FALSE) # Coerce input
  n_entries <- nrow(studies) # number of new entries
  
  # Add "doi" field if not provided
  if(!("doi" %in% names(studies))){
    studies$doi <- character(n_entries)
  }
  
  if(n_entries > 0){
    # SQL statement for insertion
    insert_study <- dbSendStatement(con,
                                    "INSERT INTO studies(abbreviation, authors, title, year, doi)
                                    VALUES(:abbreviation, :authors, :title, :year, :doi);")
    dbBind(insert_study, param=studies)
    dbClearResult(insert_study)
  }
  
  # Get assigned IDs
  if(silent == FALSE){
    study_ids <- dbGetQuery(con, "SELECT * FROM studies WHERE abbreviation = :abbreviation", param=list(abbreviation=studies$abbreviation))
    return(study_ids)
  }
}

get_study_ids <- function(query = NULL, mode = NULL){
  # query: vector with dois, abbreviations or author names of studies for which to get ids
  # mode: either "doi", "abbreviation", "author", or NULL
  
  # Remove empty values in query vector
  if(!is.null(query)){
    query <-  as.character(query[!query == ""])
  }
  
  if(is.null(mode)){
    study_ids <- dbGetQuery(con, "SELECT * FROM studies;")
  } else {
    if(mode == "doi"){
      study_ids <- dbGetQuery(con, "SELECT * FROM studies WHERE doi = ?;", param=list(query))
    }
    if(mode == "abbreviation"){
      study_ids <- dbGetQuery(con, "SELECT * FROM studies WHERE abbreviation = ?;", param=list(query))
    }
    if(mode == "author"){
      query <- paste("%", as.character(query), "%", sep = "")
      study_ids <- dbGetQuery(con, "SELECT * FROM studies WHERE authors LIKE ?;", param=list(query))
    }
  }
  
  return(study_ids[,c(1, 2, 5)])
}

get_study_info <- function(ids = NULL) {
  # id: matches study_id in studies table for which to retrieve information; not mandatory
  
  if(is.null(ids)){
    studies <- dbGetQuery(con, "SELECT * FROM studies;")
  } else {
    studies <- dbGetQuery(con, "SELECT * FROM studies WHERE study_id = ?;", param=list(ids))
  }
  return(studies)
}



get_assessment_ids <- function(query = NULL, mode = NULL ){
  # query: vector of assessor ids or dates
  # mode: either assessor_id, date, before, after
  
  if(!is.null(query)){
    query <-  as.character(query[!query == ""])
  }
  
  if(is.null(mode)){
    assessment_ids <- dbGetQuery(con, "SELECT * FROM assessments;")
  } else {
    if(mode == "assessor_id"){
      assessment_ids <- dbGetQuery(con, "SELECT * FROM assessments WHERE assessor_id = ?;", param=list(query))
    }
    if(mode == "date"){
      assessment_ids <- dbGetQuery(con, "SELECT * FROM assessments WHERE date_entered = ?;", param=list(query))
    }
    if(mode == "before"){
      assessment_ids <- dbGetQuery(con, "SELECT * FROM assessments WHERE date_entered <= ?;", param=list(query))
    }
    if(mode == "after"){
      assessment_ids <- dbGetQuery(con, "SELECT * FROM assessments WHERE date_entered >= ?;", param=list(query))
    }
  }
  
  return(assessment_ids)
}





new_studies <- function(studies, silent = FALSE){
  # studies: either a named list (for one entry) or a data.frame (for several
  # entries) with mandatory fields "abbreviation ", "authors", "title", and
  # "year"; and optional field "doi"
  
  studies <- data.frame(lapply(studies, as.character), stringsAsFactors = FALSE) # Coerce input
  n_entries <- nrow(studies) # number of new entries
  
  # Add "doi" field if not provided
  if(!("doi" %in% names(studies))){
    studies$doi <- character(n_entries)
  }
  
  if(n_entries > 0){
    # SQL statement for insertion
    insert_study <- dbSendStatement(con,
                                    "INSERT INTO studies(abbreviation, authors, title, year, doi)
                                    VALUES(:abbreviation, :authors, :title, :year, :doi);")
    dbBind(insert_study, param=studies)
    dbClearResult(insert_study)
  }
  
  # Get assigned IDs
  if(silent == FALSE){
    study_ids <- dbGetQuery(con, "SELECT * FROM studies WHERE abbreviation = :abbreviation", param=list(abbreviation=studies$abbreviation))
    return(study_ids)
  }
}


new_assessment <- function(assessor_id, source = NULL, date = NULL){
  # assessor_id: assessor id as registered in assessors table
  # source: source published source of evidence assessments; not mandatory
  # date: date of assessment; if not provided, will be replaced with today's
  # date; must be entered as YYYY-MM-DD
  
  if(is.null(source)){
    source <- ""
  }
  
  if(is.null(date)){
    date <- as.character(Sys.Date())
  }
  
  # Enable foreign key constraints
  dbExecute(con, "PRAGMA foreign_keys = ON;")
  
  # SQL statement for insertion
  insert_assessment <- dbSendStatement(con,
                                       "INSERT INTO assessments(assessor_id, date_entered, source)
                                       VALUES(?, ?, ?);")
  dbBind(insert_assessment, param=list(assessor_id, date, source))  
  dbClearResult(insert_assessment)
}

# For Compatability with MySQL: use ? as placeholders and reformat column order of data.frames

# Add/extend possibility to detect duplicates in new_assessor and new_study (fuzzy matchings, dois) [get ids of duplicate records and return]