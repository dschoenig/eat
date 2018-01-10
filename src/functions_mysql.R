# TODO: Ensure functionality with MySQL

# R functions to interact with the evidence assessment database

library(DBI)
library(tidyr)

new_assessors <- function(assessors, silent = FALSE){
  # assessors: either a named list (for one entry) or a data.frame (for several
  # entries) with mandatory fields "name" and "email"
  
  assessors <- data.frame(lapply(assessors, as.character), stringsAsFactors = FALSE) # Coerce input
  
  # Check whether assessors are already in database and remove duplicates from entry dataframe
  duplicates <- dbGetQuery(con, "SELECT * FROM assessors WHERE name = :name;", param=list(name=assessors$name))
  if(nrow(duplicates) != 0){
    duplicate_assessors <- unique(duplicates$name)
    assessors_new <- subset(assessors, !(assessors$name %in% duplicate_assessors))
    warning("One or more assessors are already registered.")
  } else {
    assessors_new <-  assessors 
  }
  
  n_entries <- nrow(assessors_new) # number of new entries
  
  if(n_entries > 0){
      # SQL statement for insertion
      insert_assessor <- dbSendStatement(con,
                                         "INSERT INTO assessors(name, email)
                                         VALUES(:name, :email);")
      dbBind(insert_assessor, param=assessors_new)  
      dbClearResult(insert_assessor)
  }
  
  # Get assigned IDs
  if(silent == FALSE){
    assessor_ids <- dbGetQuery(con, "SELECT * FROM assessors WHERE name = :name;", param=list(name=assessors$name))
    return(assessor_ids)
  }
}

get_assesor_ids <- function(name = NULL){
  if(is.null(name)){
    assessors <- dbGetQuery(con, "SELECT * FROM assessors;")
  } else {
    name <- paste("%", as.character(name), "%", sep = "")
    assessors <- dbGetQuery(con, "SELECT * FROM assessors WHERE name LIKE :name;", param=list(name = name))
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

assessment_id=1

assess_study <- function(studies, assessment_id, answers, silent = FALSE){
  # studies: named list or data.frame with mandatory fields "study_id" (matches
  # "study_id" in "studies" table), "study_design", "res_context", "res_focus",
  # and "res_outcome"
  # assessment_id: matching "assessment_id" in the "assessments" table
  # answers: data.frame with on row per study to be assessed, provided in the
  # same order as for "studies"; column number has to match question_id; answers
  # are 1 for "yes", 0 for "no" and NA in case question does not apply
  
  n_entries <- nrow(studies) # number of new entries
  studies <- data.frame(lapply(studies, as.character), stringsAsFactors = FALSE)
  studies$assessment_id <- rep(as.character(assessment_id), n_entries)
  # Define order to use ? placeholders
  studies <- select(studies, study_id, study_design, res_context, res_focus, res_question, res_outcome, assessment_id)
  
  ## Preliminary LoE  
  # look up preliminary LoE based on study design
  loe_pre <- dbGetQuery(con, "SELECT loe_pre FROM study_designs WHERE study_design = ?;", param=unname(studies[2]))
  
  
  ## Calculate quality scores
  # reformat answers data.frame
  answers[which(answers!=1 & answers != 0, arr.ind = T)] <- NA
  answers[is.na(answers)] <- "NA"
  answers <- data.frame(lapply(answers, as.character), stringsAsFactors = FALSE)
  n_questions <- as.integer(dbGetQuery(con, "SELECT count(question_id) FROM checklist;"))
  names(answers) <- 1:n_questions
  
  # scores
  points_p <- rowSums(answers == 0 | answers == 1)
  points_q <- rowSums(answers == 1)
  q_score <- round(points_q/points_p, 4) * 100
  
  ## Look up downgrading
  q_score_query <- q_score
  q_score_query[is.nan(q_score)] <- "-9" # placeholder value for NaN
  adjustments <- dbGetQuery(con, "SELECT adjustment_id, adjustment AS downgrading FROM adjustments WHERE q_score_ub >= ? AND q_score_lb < ?;", param=list(q_score_query, q_score_query))
  loe_final <- dbGetQuery(con, "SELECT loe_final FROM downgrading WHERE adjustment_id = ? AND loe_pre = ?;", param=unname(cbind(adjustments$adjustment_id, loe_pre)))
  
  ## combine results
  loe <- cbind(studies[,c(7,1:6)], loe_final, loe_pre, points_p, points_q, q_score, downgrading=adjustments$downgrading, reviewed = rep("no",n_entries))
  loe <- data.frame(lapply(loe, as.character), stringsAsFactors = FALSE)
  
  
  ## Enter records
  loe_no_nan <- loe[loe$q_score != "NaN",]
  loe_nan <- loe[loe$q_score == "NaN",]
  
  # enter records with valid quality score
  
  dbExecute(con, "PRAGMA foreign_keys = ON;") # Enable foreign key constraints
  assess_study <- dbSendStatement(con,
                                  "INSERT INTO level_of_evidence(assessment_id, study_id, study_design, 
                                  res_context, res_focus, res_question, res_outcome, loe_final, loe_pre,                                          points_p, points_q, q_score, downgrading, reviewed)
                                  VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
  dbBind(assess_study, params = unname(loe_no_nan))
  dbClearResult(assess_study)
  
  # enter records without quality score
  dbExecute(con, "PRAGMA foreign_keys = ON;") # Enable foreign key constraints
  assess_study <- dbSendStatement(con,
                                  "INSERT INTO level_of_evidence(assessment_id, study_id, study_design, 
                                  res_context, res_focus, res_question, res_outcome, loe_final, loe_pre,                                          points_p, points_q, q_score, downgrading, reviewed)
                                  VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, ?, ?);")
  dbBind(assess_study, params = unname(loe_nan[,-12]))
  dbClearResult(assess_study)
  
  ## enter detailed results of quality assessment
  quality <- cbind(assessment_id = as.numeric(studies[,7]), study_id = as.numeric(studies[,1]), answers)
  quality <- gather(quality, key="question_id", value="answer", -assessment_id, -study_id)
  quality <- arrange(quality, assessment_id, study_id)
  quality$answer[which(quality$answer == 1)] <- "yes"
  quality$answer[which(quality$answer == 0)] <- "no"
  
  enter_answers <- dbSendStatement(con,
                                  "INSERT INTO quality(assessment_id, study_id, question_id, answer)
                                  VALUES(?, ?, ?, ?);")
  dbBind(enter_answers, params = unname(quality))
  dbClearResult(assess_study)
  
  
  
  if(silent == FALSE){
    records <- dbGetQuery(con, 
                            "SELECT record_id, assessment_id, study_id, loe_pre, loe_final 
                            FROM level_of_evidence WHERE study_id = ? AND assessment_id = ?;", 
                            param=unname(studies[,c(1,7)]))
    return(records)
  }
}

# For Compatability with MySQL: use ? as placeholders and reformat column order of data.frames

# Add/extend possibility to detect duplicates in new_assessor and new_study (fuzzy matchings, dois) [get ids of duplicate records and return]
