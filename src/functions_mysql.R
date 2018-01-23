# TODO: Ensure functionality with MySQL
#       Add conn argument to each function (and con <- conn)

# R functions to interact with the evidence assessment database

library(DBI)
library(stringdist)
library(tidyr)


############################ ##
# FUNCTIONS FOR DATA ENTRY ####
############################ ##

CreateStudies <- function(studies, conn=eaDB){
  
  # Format input data
  input <- studies
  n <- nrow(input)
  studies <- TemplateStudies(n)
  studies$abbreviation <- as.character(input$abbreviation)
  studies$authors <- as.character(input$authors)
  studies$title <- as.character(input$title)
  studies$year <- as.integer(input$year)
  studies$doi <- as.character(input$doi)
  
  studies_new <- studies
  n_entries <- nrow(studies_new) # number of new entries
  
  if(n_entries > 0){
    # SQL statement for insertion
    insert_study <- dbSendStatement(conn,
                                    "INSERT INTO studies(abbreviation,
                                                         authors,
                                                         title,
                                                         year,
                                                         doi)
                                          VALUES (?, ?, ?, ?, ?);")
    dbBind(insert_study, param=list(studies_new$abbreviation, 
                                    studies_new$authors,
                                    studies_new$title,
                                    studies_new$year,
                                    studies_new$doi))  
    dbClearResult(insert_study)
  }
  
  # Get assigned IDs
  study_ids <- dbGetQuery(conn, "SELECT * FROM studies WHERE abbreviation = ?;", param=list(studies$abbreviation))
  return(study_ids)
}

CreateAssessors <- function(assessors, conn=eaDB){
  # Creates new rows in the assessor table
  #
  # Args
  #   assessors: Either a named list (for one entry) or a data.frame (for 
  #       several entries) with mandatory fields "name" and "email"
  #
  # Returns: 
  #   Those rows of the modified assessor table that correspond to the input
  #   data.
  
  # Format input data
  input <- assessors
  n <- nrow(input)
  assessors <- TemplateAssessors(N=n)
  assessors$name <- as.character(input$name)
  assessors$email <- as.character(input$email)
  
  
  # Check whether assessors are already in database and remove duplicates from 
  # entry dataframe
  duplicates <- dbGetQuery(conn, "SELECT * FROM assessors WHERE name = ?;", 
                           param=list(assessors$name))
  if(nrow(duplicates) != 0){
    duplicate_assessors <- unique(duplicates$name)
    assessors_new <- subset(assessors, 
                            !(assessors$name %in% duplicate_assessors))
    warning("One or more assessors are already registered.","\n",
            "Only new ones will be added; duplicate ones will not be modified.")
  } else {
    assessors_new <-  assessors 
  }
  
  n_entries <- nrow(assessors_new) # number of new entries
  
  if(n_entries > 0){
      # SQL statement for insertion
      insert_assessor <- dbSendStatement(conn,
                                         "INSERT INTO assessors(name, email)
                                               VALUES (?, ?);")
      dbBind(insert_assessor, param=list(assessors_new$name, 
                                         assessors_new$email))  
      dbClearResult(insert_assessor)
  }
  
  # Get assigned IDs
  assessor_ids <- dbGetQuery(conn, "SELECT * FROM assessors WHERE name = ?;", param=list(assessors$name))
  return(assessor_ids)
}

CreateAssessments <- function(assessments, date=NULL, conn=eaDB){
  
  # If date is not specified, set today's date
  if(is.null(date)){
    date <- Sys.Date()
  }
  
  # Format input data
  input <- assessments
  n <- nrow(input)
  assessments <- TemplateAssessments(n)
  assessments$assessor_id <- as.integer(input$assessor_id)
  assessments$source <- as.character(input$source)
  assessments$date_entered <- date
  
  assessments_new <- assessments
  n_entries <- nrow(assessments_new) # number of new entries
  
  if(n_entries > 0){
    # SQL statement for insertion
    insert_assessment <- dbSendStatement(conn,
                                    "INSERT INTO assessments(assessor_id,
                                                             source,
                                                             date_entered)
                                    VALUES (?, ?, ?);")
    dbBind(insert_assessment, param=list(assessments_new$assessor_id, 
                                         assessments_new$source,
                                         assessments_new$date_entered))
    dbClearResult(insert_assessment)
  }
  
  # Get assigned IDs
  assessment_ids <- dbGetQuery(conn, "SELECT * FROM assessments WHERE assessor_id = ? AND date_entered = ?;", param=list(assessments$assessor_id, assessments$date_entered))
  return(assessment_ids)
}

# Different workflow: enter answers to questions first, then perform calculations
conn=eaDB
assessment_id=1
AssessStudies <- function(studies, assessment_id, conn=eaDB){
  
  n_questions <- as.integer(dbGetQuery(conn, "SELECT COUNT(*) FROM checklist")[1,1])
  c_questions <- paste0("q",seq(1:n_questions))
    
  # Format input data
  input <- studies
  n <- nrow(input)
  studies <- TemplateAssessStudies(n, n_questions)
  studies$study_id <- as.integer(input$study_id)
  studies$study_design <- as.character(input$study_design)
  studies$res_context <- as.character(input$res_context)
  studies$res_focus <- as.character(input$res_focus)
  studies$res_question <- as.character(input$res_question)
  studies$res_outcome <- as.character(input$res_outcome)
  for(q in c_questions){
    answers <- subset(input, select = noquote(q))[,1]
    col <- which(colnames(studies) == q)
    studies[, col] <- answers
  }
  studies$assessment_id <- as.integer(assessment_id)
  
  # Convert answers to "yes" "no" and "NA", regardless of original NA value
  c_id_questions <- which(colnames(studies) %in% c_questions)
  answers <- as.data.frame(matrix(NA , n, n_questions))
  colnames(answers) <- c_questions
  ids_yes <- which(studies[,c_id_questions] == 1, arr.ind=T)
  ids_no <- which(studies[,c_id_questions] == 0, arr.ind=T)
  answers[ids_yes] <- "yes"
  answers[ids_no] <- "no"
  answers[is.na(answers)] <- "NA"
  studies[,c_id_questions] <- answers
  
  # Prepare checklist answers for database insertion
  quality <- cbind(assessment_id=studies$assessment_id, study_id=studies$study_id, 
        studies[,c_id_questions])
  quality <- gather(quality, key="question_id", value="answer", -assessment_id, -study_id)
  quality$question_id <- gsub("q", "", quality$question_id)
  ord <- order(as.integer(quality$assessment_id), 
               as.integer(quality$study_id), 
               as.integer(quality$question_id))
  quality <- quality[ord,]
  
  # Check if studies have alredy been assessed
  check <- dbGetQuery(conn, "SELECT assessment_id, study_id 
                               FROM quality 
                              WHERE assessment_id = ? AND study_id = ?;", 
                      params=list(quality$assessment_id, quality$study_id))
  duplicates <- unique(check)
  
  # Stop if studies have already been assessed
  if(nrow(duplicates) != 0){
    if(nrow(duplicates) == 1){
      sp <- "study with ID '"
    } else {
      sp <- "studies with IDs '"
    }
    stop(paste0("Assessment with ID '", assessment_id, 
                "' already contains answers to checklist questions for ",
                sp, 
                paste(duplicates$study_id, collapse = "', '"), 
                "'. Please remove these studies from the input data and try again. Alternatively, remove corresponding answers from the 'quality' table."))
  }
  
  # Enter checklist answers
  enter_answers <- dbSendStatement(conn,
                                   "INSERT INTO quality(assessment_id, 
                                                        study_id, 
                                                        question_id, 
                                                        answer)
                                         VALUES(?, ?, ?, ?);")
  dbBind(enter_answers, params = list(quality$assessment_id, 
                                      quality$study_id, 
                                      quality$question_id, 
                                      quality$answer))
  dbClearResult(enter_answers)

  # look up preliminary LoE based on study design
  loe_pre <- dbGetQuery(conn, "SELECT loe_pre 
                                 FROM study_designs 
                                WHERE study_design = ?;", 
                        param=list(studies$study_design))
  
  # Check whether a preliminary LoE could be assigned to each study
  if(nrow(loe_pre) != nrow(studies)){
    designs <- dbGetQuery(conn, "SELECT DISTINCT study_design FROM study_designs;")
    stop(paste0("One or more study designs could not been assigned a preliminary LoE. Please assure that the study design provided for each study is one of the following: '", paste(designs$study_design, collapse = "', '","'"), sep = ""))
  }
  
  # Add assigned prliminary LoE
  studies$loe_pre <- loe_pre$loe_pre
  
  # Calculate quality scores
  qscores <- dbGetQuery(conn, "SELECT study_id, 
                                      n_yes + n_no AS 'points_p',
                                      n_yes AS 'points_q',
                                      n_yes / (n_yes + n_no) AS 'q_score'
                                 FROM (SELECT study_id,
                                              assessment_id,
                                              COUNT(CASE WHEN answer = 'yes' THEN 1 END) as n_yes,
                                              COUNT(CASE WHEN answer = 'no' THEN 1 END) as n_no
                                         FROM quality
                                        WHERE assessment_id = ? AND study_id = ?
                                     GROUP BY study_id, assessment_id
                                      ) AS quality_ag;",
           params=list(studies$assessment_id, studies$study_id))
  studies$points_p <- qscores$points_p
  studies$points_q <- qscores$points_q
  studies$q_score <- qscores$q_score[match(studies$study_id, qscores$study_id)] * 100
  
  # Downgrade LoE according to quality score
  studies$q_score[is.na(studies$q_score)] <- -9  # placeholder for calculation
  adjustments <- dbGetQuery(conn, "SELECT adjustment_id, 
                                          adjustment AS downgrading 
                                     FROM adjustments 
                                    WHERE q_score_ub >= ? AND q_score_lb < ?;", 
                            param=list(studies$q_score, studies$q_score))
  studies$downgrading <- adjustments$downgrading
  loe_final <- dbGetQuery(conn, "SELECT loe_final 
                                   FROM downgrading 
                                  WHERE adjustment_id = ? AND loe_pre = ?;", 
                          param=list(adjustments$adjustment_id, studies$loe_pre))
  studies$loe_final <- loe_final$loe_final
  studies$q_score[studies$q_score == -9] <- "NULL"
  
  studies_n <- studies[1,]
  i=1
  # Enter complete information into level_of_evidence table
  assess_study <- dbSendStatement(conn,"INSERT INTO level_of_evidence(
                                               assessment_id, 
                                               study_id, 
                                               study_design, 
                                               res_context, 
                                               res_focus, 
                                               res_question, 
                                               res_outcome, 
                                               loe_final, 
                                               loe_pre,
                                               points_p, 
                                               points_q, 
                                               q_score, 
                                               downgrading, 
                                               reviewed)
                                  VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'no');")


  for(i in 1:nrow(studies)){
    i=1
    dbBind(assess_study, params = list(studies$assessment_id[i], 
                                       studies$study_id[i], 
                                       studies$study_design[i], 
                                       studies$res_context[i], 
                                       studies$res_focus[i], 
                                       studies$res_question[i], 
                                       studies$res_outcome[i], 
                                       studies$loe_final[i], 
                                       studies$loe_pre[i],
                                       as.integer(studies$points_p)[i], # remove?
                                       as.integer(studies$points_q)[i], 
                                       studies$q_score[i]), 
                                       studies$downgrading[i]))
  }
  
  dbClearResult(assess_study)
  
  }


AssessStudy_OLD <- function(studies, assessment_id, answers, silent = FALSE){
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


################################ ##
# FUNCTIONS FOR DATA RETRIEVAL ####
################################ ##


GetRecords <- function(select=NULL, field=NULL, table, 
                       mode="similar", fuzzy.th=0.25, conn=eaDB) {
  
  # Check for existance of table
  if(!table %in% dbListTables(conn)){
    stop(paste0("There is no table called '", table, "' in the database."))
  }
  # If field is provided, check for existance;
  if(!is.null(field)){
    fields <- dbGetQuery(conn, paste0("SHOW COLUMNS FROM ", 
                                      dbQuoteIdentifier(conn, table), ";"))
    if(!field %in% fields$Field){
      stop(paste0("There is no field called '", field, "' in table '", 
                  table, "'."))
    }
  }
  # If field name is provided but no query term, print warning that entire table
  # will be returned
  if(is.null(select) && !is.null(field)){
    warning(paste0("No query term provided for field '", field, 
                   "'. Entire table will be returned."))
    field  <-  NULL
  }
  # If query term is provided but no field name, print warning that entire table
  # will be returned
  if(!is.null(select) && is.null(field)){
    warning(paste0("No field name provided for query term '", select, 
                   "'. Entire table will be returned."))
    select  <-  NULL
  }
  # Return entire table if no query is entered; else perform query
  if(is.null(select) && is.null(field)){
    query <- paste0("SELECT * FROM ", 
                    dbQuoteIdentifier(conn, table), ";")
    results <- dbGetQuery(conn, query)
    return(results)
  } else {
    
    # Perform query depending on query mode
    # Query table
    if(mode=="similar"){
      select <- paste("%", as.character(select), "%", sep = "")
      query <- paste0("SELECT * FROM ", 
                      dbQuoteIdentifier(conn, table), 
                      " WHERE ", 
                      dbQuoteIdentifier(conn, field), 
                      " LIKE ?;")
      results <- dbGetQuery(conn, query, params=list(select))
      return(results)
    }
    
    if(mode == "exact"){
      query <- paste0("SELECT * FROM ", 
                      dbQuoteIdentifier(conn, table), 
                      " WHERE ", 
                      dbQuoteIdentifier(conn, field), 
                      " = ?;")
      results <- dbGetQuery(conn, query, params=list(select))
      return(results)
    }
    
    if(mode == "fuzzy"){
      if(length(select) > 1){
        stop("For fuzzy matching, please provide only one query term.")
      }
      query <- paste0("SELECT * FROM ", 
                      dbQuoteIdentifier(conn, table))
      res <- dbSendQuery(conn, query)
      res_part <- dbFetch(res, n=50)
      if(nrow(res_part) > 0) {
        res_part_q <- subset(res_part, select=noquote(field))[,1]
        dist_jw <- stringdist(select, res_part_q, method="jw", p=0.1)
        res_part <- cbind(distance=round(dist_jw, 2), res_part)
        results <- subset(res_part, distance <= fuzzy.th)
        repeat {
          res_part <- dbFetch(res, n=2)
          if(nrow(res_part) <= 1) {
            break
          }
          res_part_q <- subset(res_part, select=noquote(field))[,1]
          dist_jw <- stringdist(select, res_part_q, method="jw", p=0.1)
          res_part <- cbind(distance=round(dist_jw, 2), res_part)
          results <- rbind(results, subset(res_part, distance <= fuzzy.th))
        }
        results <- results[order(results$distance), ]
        dbClearResult(res)
        return(results)
      } else {
        dbClearResult(res)
        return(res_part)
      }
    }
  }
}

GetStudies <- function(select=NULL, field=NULL, 
                       mode="similar", fuzzy.th=0.25, conn=eaDB){
  
  results <- GetRecords(select=select, field=field, table="studies", 
                        mode=mode, fuzzy.th=fuzzy.th, conn=conn)
  
  return(results)
}

GetAssessors <- function(select=NULL, field=NULL, 
                         mode="similar", fuzzy.th=0.25, conn=eaDB){
  
  results <- GetRecords(select=select, field=field, table="assessors", 
             mode=mode, fuzzy.th=fuzzy.th, conn=conn)
  return(results)
  
}

GetAssessments <- function(select=NULL, field=NULL, 
                           mode="similar", fuzzy.th=0.25, conn=eaDB){
  
  results <- GetRecords(select=select, field=field, table="assessments", 
                        mode=mode, fuzzy.th=fuzzy.th, conn=conn)
  
  return(results)
}


###################### ##
# Template Functions ####
###################### ##

TemplateStudies <- function(N=1){
  studies <- data.frame("abbreviation" = character(N), 
                        "authors" = character(N), 
                        "title" = character(N), 
                        "year" = integer(N), 
                        "doi" = character(N))
  return(studies)
} 

TemplateAssessors <- function(N=1){
  assessors <- data.frame("name" = character(N), 
                          "email" = character(N))
  return(assessors)
}

TemplateAssessments <- function(N=1){
  assessments <- data.frame("assessor_id" = integer(N), 
                            "source" = character(N))
  return(assessments)
}

TemplateAssessStudies <- function(N=1, no.cl.questions=43){
  studies_details <- data.frame("study_id" = integer(N), 
                            "study_design" = character(N),
                            "res_context" = character(N),
                            "res_focus" = character(N),
                            "res_question" = character(N),
                            "res_outcome" = character(N))
  
  studies_checklist <- matrix(NA, N, no.cl.questions)
  studies_checklist <- as.data.frame(studies_checklist)
  colnames(studies_checklist) <- paste0("q", seq(1:no.cl.questions))
  
  return(cbind(studies_details, studies_checklist))
}


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
