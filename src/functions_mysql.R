# These functions are built to interact with a database for evidence assessment,
# set up according to Mupepele et al. (2016). The functions serve as building
# blocks for a CRUD interface and cover data entry, retrieval, updating and
# removal, as well as duplicate handling.
#
# The R interface depends on a correctly structured and initialized MySQL or
# MariaDB database. Please refer to the SQL documentation for the eveidence
# assessment database to ensure that your database has been set up correctly.
# Some functions will not work for database users with restricted rights. In
# particular, all `CombineDuplicates` functons, all `Remove` functions, and
# `ReassessStudies` require the user to possess `DELETE` rights. The interface
# functions have been built mainly on top of the `DBI` and `RMariaDB` packages.
# These packages offer general purpose functions to interact with the evidence
# assessment database in ways that are not covered by this R interface.
#
# For general attribution of the evidence assessment database and the assessment
# procedure, please use:
#
# Mupepele, A.-C., Walsh, J. C., Sutherland, W. J., & Dormann, C. F. (2016). An
# evidence assessment tool for ecosystem services and conservation studies.
# Ecological Applications, 26(5), 1295–1301. https://doi.org/10.1890/15-0595

# Check for installed packages
all(c("DBI", "RMariaDB", "stringdist", "tidyr") %in% rownames(installed.packages()))

# Load packages
library(DBI)
library(stringdist)
library(tidyr)


############################ ##
# FUNCTIONS FOR DATA ENTRY ####
############################ ##

CreateStudies <- function(studies, force=FALSE, conn=eaDB){
  
  # Format input data
  input <- studies
  n <- nrow(input)
  studies <- TemplateStudies(n)
  studies$abbreviation <- as.character(input$abbreviation)
  studies$authors <- as.character(input$authors)
  studies$title <- as.character(input$title)
  studies$year <- as.integer(input$year)
  studies$doi <- as.character(input$doi)
  
  if(force == FALSE){
    # Raise error if duplicates are present
    duplicates <- CheckForDuplicateStudies(input)
    if(nrow(duplicates) != 0){
      stop(paste0("Duplicate entries found. Use 'CheckForDuplicateStudies(studies)' on the 'studies' data frame you have provided."))
    }
  }
  
  studies_new <- studies
  n_entries <- nrow(studies_new) # number of new entries
  
  # Get IDs of studies already in database
  old_ids <- dbGetQuery(conn=conn, "SELECT study_id FROM studies;")[,1]
  
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
  
  # Get IDs of studies in database after insertion
  new_ids <- dbGetQuery(conn=conn, "SELECT study_id FROM studies;")[,1]
  # Which ones have been added?
  added_ids <- new_ids[which(!new_ids %in% old_ids)]
  
  # Return newly added studies
  studies_added <- dbGetQuery(conn, "SELECT * FROM studies
                                             WHERE study_id = ?;",
                              param=list(added_ids))
  return(studies_added)
}

CreateAssessors <- function(assessors, force=FALSE, conn=eaDB){
  
  # Format input data
  input <- assessors
  n <- nrow(input)
  assessors <- TemplateAssessors(N=n)
  assessors$name <- as.character(input$name)
  assessors$email <- as.character(input$email)
  
  if(force == FALSE){
    # Raise error if duplicates are present
    duplicates <- CheckForDuplicateAssessors(input)
    if(nrow(duplicates) != 0){
      stop(paste0("Duplicate entries found. Use 'CheckForDuplicateAssessors(assessors)' on the 'assessors' data frame you have provided."))
    }
  }
  
  assessors_new <- assessors
  n_entries <- nrow(assessors_new) # number of new entries
  
  # Get IDs of assessors already in database
  old_ids <- dbGetQuery(conn=conn, "SELECT assessor_id FROM assessors;")[,1]
  
  if(n_entries > 0){
    # SQL statement for insertion
    insert_assessor <- dbSendStatement(conn,
                                       "INSERT INTO assessors(name, email)
                                             VALUES (?, ?);")
    dbBind(insert_assessor, param=list(assessors_new$name,
                                       assessors_new$email))
    dbClearResult(insert_assessor)
  }
  
  # Get IDs of assessors in database after insertion
  new_ids <- dbGetQuery(conn=conn, "SELECT assessor_id FROM assessors;")[,1]
  # Which ones have been added?
  added_ids <- new_ids[which(!new_ids %in% old_ids)]
  
  # Get assigned IDs
  assessor_added <- dbGetQuery(conn, "SELECT * FROM assessors
                                              WHERE assessor_id = ?;",
                               params=list(added_ids))
  return(assessor_added)
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
  assessments$assessor_id <- as.integer(as.character(input$assessor_id))
  assessments$source <- as.character(input$source)
  assessments$date_entered <- date
  
  assessments_new <- assessments
  n_entries <- nrow(assessments_new) # number of new entries
  
  # Get IDs of assessments already in database
  old_ids <- dbGetQuery(conn=conn, "SELECT assessment_id FROM assessments;")[,1]
  
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
  
  # Get IDs of assessments in database after insertion
  new_ids <- dbGetQuery(conn=conn, "SELECT assessment_id FROM assessments;")[,1]
  # Which ones have been added?
  added_ids <- new_ids[which(!new_ids %in% old_ids)]
  
  # Get assigned IDs
  assessments_added <- dbGetQuery(conn, "SELECT * FROM assessments
                                                 WHERE assessment_id = ?;",
                                  param=list(added_ids))
  return(assessments_added)
}

AssessStudies <- function(studies, assessment.id, conn=eaDB){
  # Get info on checklist
  n_questions <- as.integer(dbGetQuery(conn, "SELECT COUNT(*) FROM checklist")[1,1])
  c_questions <- paste0("q",seq(1:n_questions))
  
  # Format input data
  input <- studies
  n <- nrow(input)
  studies <- TemplateAssessStudies(n, n_questions)
  studies$study_id <- as.integer(as.character(input$study_id))
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
  studies$assessment_id <- as.integer(assessment.id)
  
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
    stop(paste0("Assessment with ID '", assessment.id,
                "' already contains answers to checklist questions for studies with the following IDs: ",
                paste(duplicates$study_id, collapse = "', '"),
                "'. Please remove these studies from the input data and try again. Alternatively, remove corresponding records from the 'level_of_evidence' table. You can overwrite existing assessments with 'ReassessStudies()'."))
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
  
  # Add assigned preliminary LoE
  studies$loe_pre <- loe_pre$loe_pre
  
  # Calculate quality scores; calculations are performed at the database level
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
  studies$q_score[studies$q_score == -9] <- NA  # Reset to NA
  
  # Enter complete information into level_of_evidence table; NA entries in R
  # data frames will be entered as NULL in database
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
  dbBind(assess_study, params = list(studies$assessment_id,
                                     studies$study_id,
                                     studies$study_design,
                                     studies$res_context,
                                     studies$res_focus,
                                     studies$res_question,
                                     studies$res_outcome,
                                     studies$loe_final,
                                     studies$loe_pre,
                                     as.integer(studies$points_p),
                                     as.integer(studies$points_q),
                                     studies$q_score,
                                     studies$downgrading))
  dbClearResult(assess_study)
  
  # Get part of table for return
  new_records <- dbGetQuery(conn, "SELECT record_id,
                                          assessment_id,
                                          study_id,
                                          study_design,
                                          loe_pre, points_p,
                                          points_q, q_score,
                                          downgrading,
                                          loe_final
                                     FROM level_of_evidence
                                    WHERE assessment_id = ? AND study_id = ?;",
                            params=list(studies$assessment_id, studies$study_id))
  return(new_records)
}


################################ ##
# FUNCTIONS FOR DATA RETRIEVAL ####
################################ ##


GetRecords <- function(select=NULL, field=NULL, table, return.fields=NULL, ids.only=FALSE,
                       mode="exact", fuzzy.min.sim=0.75, conn=eaDB) {
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
  
  # set field identifier
  if(!is.null(return.fields) && ids.only == TRUE){
    warning("Returning only IDs. Set 'ids.only = FALSE' (Default) to return all or specific query fields.")
    return.fields <- NULL
  }
  
  if(!is.null(return.fields)){
    columns <- paste0(dbQuoteIdentifier(conn, return.fields), collapse=", ")
  } else {
    columns <- "*"
  }
  
  if(is.null(select) && is.null(field)){
    # Return entire table if no query is entered; else perform query
    query <- paste0("SELECT ",
                    columns,
                    " FROM ",
                    dbQuoteIdentifier(conn, table), ";")
    results <- dbGetQuery(conn, query)
  } else {
    # Perform query depending on query mode
    if(mode=="partial"){
      select <- paste("%", as.character(select), "%", sep = "")
      query <- paste0("SELECT ",
                      columns,
                      " FROM ",
                      dbQuoteIdentifier(conn, table),
                      " WHERE ",
                      dbQuoteIdentifier(conn, field),
                      " LIKE ?;")
      results <- dbGetQuery(conn, query, params=list(select))
    }
    
    if(mode == "exact"){
      query <- paste0("SELECT ",
                      columns,
                      " FROM ",
                      dbQuoteIdentifier(conn, table),
                      " WHERE ",
                      dbQuoteIdentifier(conn, field),
                      " = ?;")
      results <- dbGetQuery(conn, query, params=list(select))
    }
    
    if(mode == "fuzzy"){
      if(length(select) > 1){
        stop("For fuzzy matching, please provide only one query term.")
      }
      fuzzy.th <- 1 - fuzzy.min.sim
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
      } else {
        dbClearResult(res)
        results <- res_part
        results$distance <- numeric()
      }
      
      if(!is.null(return.fields)){
        # Select return fields
        results <- subset(results, select=c("distance", return.fields))
      }
      
      # Convert distance into similarity
      names(results)[names(results) == "distance"] <- "similarity"
      results$similarity <- 1 - results$similarity
    }
  }
  
  if(ids.only == TRUE){
    results <- IDsOnly(results)
  }
  return(results)
}

GetStudies <- function(select=NULL, field=NULL, return.fields=NULL, ids.only=FALSE,
                       mode="exact", fuzzy.min.sim=0.75, conn=eaDB){
  
  results <- GetRecords(select=select, field=field, table="studies",
                        return.fields=return.fields, ids.only = ids.only,
                        mode=mode, fuzzy.min.sim=fuzzy.min.sim, conn=conn)
  return(results)
}

GetAssessors <- function(select=NULL, field=NULL, return.fields=NULL, ids.only=FALSE,
                         mode="exact", fuzzy.min.sim=0.75, conn=eaDB){
  
  results <- GetRecords(select=select, field=field, table="assessors",
                        return.fields=return.fields, ids.only = ids.only,
                        mode=mode, fuzzy.min.sim=fuzzy.min.sim,
                        conn=conn)
  return(results)
  
}

GetAssessments <- function(select=NULL, field=NULL, return.fields=NULL, ids.only=FALSE,
                           mode="exact", fuzzy.min.sim=0.75, conn=eaDB){
  
  results <- GetRecords(select=select, field=field, table="assessments",
                        return.fields=return.fields, ids.only = ids.only,
                        mode=mode, fuzzy.min.sim=fuzzy.min.sim, conn=conn)
  
  return(results)
}

GetLoE <- function(select=NULL, field=NULL, return.fields=NULL, ids.only=FALSE,
                   mode="exact", fuzzy.min.sim=0.75, conn=eaDB){
  
  results <- GetRecords(select=select, field=field, table="level_of_evidence",
                        return.fields=return.fields, ids.only = ids.only,
                        mode=mode, fuzzy.min.sim=fuzzy.min.sim, conn=conn)
  
  return(results)
}

GetFullRecords <- function(select=NULL, field=NULL, ids.only = FALSE,
                           mode="exact", fuzzy.min.sim=0.75, conn=eaDB){
  
  # If query term is provided but no field name, warn that entire table will be
  # returned
  if(!is.null(select) && is.null(field)){
    warning(paste0("No field name provided for query term '", select,
                   "'. Entire table will be returned."))
    select  <-  NULL
  }
  
  # Return entire table if no query is entered; else perform query
  if(is.null(select) && is.null(field)){
    results <-  dbGetQuery(conn, "SELECT record_id,
                           assessment_id,
                           study_id,
                           assessor_id,
                           abbreviation AS 'studies.abbreviation',
                           authors AS 'studies.authors',
                           title AS 'studies.title',
                           year AS 'studies.year',
                           doi AS 'studies.doi',
                           name AS 'assessors.name',
                           email AS 'assessors.email',
                           date_entered AS 'assessments.date_entered',
                           source AS 'assessments.source',
                           study_design AS 'loe.study_design',
                           res_context AS 'loe.res_context',
                           res_focus AS 'loe.res_focus',
                           res_question AS 'loe.res_question',
                           res_outcome AS 'loe.res_outcome',
                           loe_pre AS 'loe.loe_pre',
                           loe_final AS 'loe.loe_final',
                           points_p AS 'loe.points_p',
                           points_q AS 'loe.points_q',
                           q_score AS 'loe.q_score',
                           downgrading AS 'loe.downgrading',
                           reviewed AS 'loe.reviewed'
                           FROM studies
                           JOIN level_of_evidence USING(study_id)
                           JOIN assessments USING(assessment_id)
                           JOIN assessors USING(assessor_id);")
  } else {
    if(mode == "partial"){
      select <- paste("%", as.character(select), "%", sep = "")
      query <- paste0("SELECT record_id,
                      assessment_id,
                      study_id,
                      assessor_id,
                      abbreviation AS 'studies.abbreviation',
                      authors AS 'studies.authors',
                      title AS 'studies.title',
                      year AS 'studies.year',
                      doi AS 'studies.doi',
                      name AS 'assessors.name',
                      email AS 'assessors.email',
                      date_entered AS 'assessments.date_entered',
                      source AS 'assessments.source',
                      study_design AS 'loe.study_design',
                      res_context AS 'loe.res_context',
                      res_focus AS 'loe.res_focus',
                      res_question AS 'loe.res_question',
                      res_outcome AS 'loe.res_outcome',
                      loe_pre AS 'loe.loe_pre',
                      loe_final AS 'loe.loe_final',
                      points_p AS 'loe.points_p',
                      points_q AS 'loe.points_q',
                      q_score AS 'loe.q_score',
                      downgrading AS 'loe.downgrading',
                      reviewed AS 'loe.reviewed'
                      FROM studies
                      JOIN level_of_evidence USING(study_id)
                      JOIN assessments USING(assessment_id)
                      JOIN assessors USING(assessor_id)
                      WHERE ",
                      dbQuoteIdentifier(conn, field),
                      " LIKE ?;")
      results <- dbGetQuery(conn, query, params=list(select))
    }
    if(mode == "exact"){
      query <- paste0("SELECT record_id,
                      assessment_id,
                      study_id,
                      assessor_id,
                      abbreviation AS 'studies.abbreviation',
                      authors AS 'studies.authors',
                      title AS 'studies.title',
                      year AS 'studies.year',
                      doi AS 'studies.doi',
                      name AS 'assessors.name',
                      email AS 'assessors.email',
                      date_entered AS 'assessments.date_entered',
                      source AS 'assessments.source',
                      study_design AS 'loe.study_design',
                      res_context AS 'loe.res_context',
                      res_focus AS 'loe.res_focus',
                      res_question AS 'loe.res_question',
                      res_outcome AS 'loe.res_outcome',
                      loe_pre AS 'loe.loe_pre',
                      loe_final AS 'loe.loe_final',
                      points_p AS 'loe.points_p',
                      points_q AS 'loe.points_q',
                      q_score AS 'loe.q_score',
                      downgrading AS 'loe.downgrading',
                      reviewed AS 'loe.reviewed'
                      FROM studies
                      JOIN level_of_evidence USING(study_id)
                      JOIN assessments USING(assessment_id)
                      JOIN assessors USING(assessor_id)
                      WHERE ",
                      dbQuoteIdentifier(conn, field),
                      " = ?;")
      results <- dbGetQuery(conn, query, params=list(select))
    }
    if(mode == "fuzzy"){
      if(length(select) > 1){
        stop("For fuzzy matching, please provide only one query term.")
      }
      fuzzy.th <- 1 - fuzzy.min.sim
      res <- dbSendQuery(conn, "SELECT record_id,
                         assessment_id,
                         study_id,
                         assessor_id,
                         abbreviation AS 'studies.abbreviation',
                         authors AS 'studies.authors',
                         title AS 'studies.title',
                         year AS 'studies.year',
                         doi AS 'studies.doi',
                         name AS 'assessors.name',
                         email AS 'assessors.email',
                         date_entered AS 'assessments.date_entered',
                         source AS 'assessments.source',
                         study_design AS 'loe.study_design',
                         res_context AS 'loe.res_context',
                         res_focus AS 'loe.res_focus',
                         res_question AS 'loe.res_question',
                         res_outcome AS 'loe.res_outcome',
                         loe_pre AS 'loe.loe_pre',
                         loe_final AS 'loe.loe_final',
                         points_p AS 'loe.points_p',
                         points_q AS 'loe.points_q',
                         q_score AS 'loe.q_score',
                         downgrading AS 'loe.downgrading',
                         reviewed AS 'loe.reviewed'
                         FROM studies
                         JOIN level_of_evidence USING(study_id)
                         JOIN assessments USING(assessment_id)
                         JOIN assessors USING(assessor_id);")
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
      } else {
        dbClearResult(res)
        results <- res_part
      }
      
      # Convert distance into similarity
      names(results)[names(results) == "distance"] <- "similarity"
      results$similarity <- 1 - results$similarity
    }
  }
  
  if(ids.only == TRUE){
    results <- IDsOnly(results)
  }
  return(results)
}

GetUnassessedStudies <- function(ids.only=FALSE, conn=eaDB){
  # Get IDs from studies table and level_of_evidence table
  study_ids_studies <- dbGetQuery(conn, "SELECT study_id FROM studies;")
  study_ids_loe <- dbGetQuery(conn, "SELECT DISTINCT study_id
                                                FROM level_of_evidence")
  study_ids_studies <- study_ids_studies[,1]
  study_ids_loe <- study_ids_loe[,1]
  
  # match IDs
  not_assessed <- study_ids_studies[which(!study_ids_studies %in%
                                            study_ids_loe)]
  if(ids.only == TRUE){
    return(not_assessed)
  } else {
    not_assessed_studies <- GetStudies(select=not_assessed, field="study_id",
                                       mode="exact", conn=conn)
    return(not_assessed_studies)
  }
}

GetRecordsToReview <- function(ids.only=FALSE, conn=eaDB){
  to_review <- dbGetQuery(conn, "SELECT record_id FROM level_of_evidence WHERE reviewed = 'no';")
  to_review <- to_review[,1]
  if(ids.only == TRUE){
    return(to_review)
  } else {
    to_review_records <- GetFullRecords(select=to_review, field="record_id", mode="exact",
                                        conn=conn)
    return(to_review_records)
  }
}


###################### ##
# TEMPLATE FUNCTIONS ####
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

TemplateAssessStudies <- function(N=1, n.cl.questions=43){
  studies_details <- data.frame("study_id" = integer(N),
                                "study_design" = character(N),
                                "res_context" = character(N),
                                "res_focus" = character(N),
                                "res_question" = character(N),
                                "res_outcome" = character(N))
  
  studies_checklist <- matrix(NA, N, n.cl.questions)
  studies_checklist <- as.data.frame(studies_checklist)
  colnames(studies_checklist) <- paste0("q", seq(1:n.cl.questions))
  
  return(cbind(studies_details, studies_checklist))
}

##################################### ##
# FUNCTIONS FOR HANDLING DUPLICATES ####
##################################### ##

CheckForDuplicates <- function(source=NULL, table, fields, id.field,
                               fuzzy.min.sims,
                               all.entries=FALSE, ids.only=FALSE, conn=eaDB){
  
  # set up data.frame to hold duplicates
  duplicates <- data.frame(source_row=integer(), id=integer(), duplicate_type=character(),
                           similarity=numeric(), field_matched=character(),
                           database_entry=character(), source_entry=character())
  
  # replace name for ID column
  names(duplicates)[names(duplicates) %in% "id"] <- id.field
  
  # Initilize trackers for matched IDs from database and matches rows from
  # source data.frame
  matched_ids <- numeric()
  matched_rows <- numeric()
  
  if(is.null(source)){
    # If no source provided, compare database table against itself
    source <- GetRecords(table = table)
    # Exclude matches against itself
    source_ids <- as.integer(subset(source, select=id.field)[,1])
    matched_ids_internal <- source_ids
    matched_rows_internal <- 1:nrow(source)
    internal_matching <- TRUE
  } else {
    internal_matching <- FALSE
  }
  
  # Exact matching
  for(i in 1:length(fields)){
    # Loop over fields to query
    select <- as.character(subset(source, select = fields[i])[,1])
    
    for(j in 1:length(select)){
      # Loop over individual queries
      res <- GetRecords(select=select[j], field = fields[i], table=table, mode="exact",
                        return.fields = c(id.field, fields[i]))
      
      # Combine results into data.frame
      source_row <- rep(j, nrow(res))
      id <-  as.numeric(subset(res, select=id.field)[,1])
      duplicate_type <- as.character(rep("exact", nrow(res)))
      similarity <- as.numeric(rep(1, nrow(res)))
      field_matched <- rep(fields[i], nrow(res))
      database_entry <- subset(res, select=noquote(fields[i]))[,1]
      source_entry <- rep(select[j], nrow(res))
      dup <- data.frame(source_row, id, duplicate_type, similarity, field_matched, source_entry, database_entry)
      names(dup)[names(dup) %in% "id"] <- id.field
      
      if(all.entries == FALSE){
        # remove records with ID that have already been matched
        dup_ids <- subset(dup, select=id.field)[,1]
        match_master <- which(matched_rows %in% j & matched_ids %in% dup_ids)
        rem_rows <- which(dup$source_row %in% matched_rows[match_master] &
                            dup_ids %in% matched_ids[match_master])
        if(length(rem_rows) > 0){
          dup <- dup[-rem_rows,]
        }
      }
      
      # add results to master duplicate table
      duplicates <- rbind(dup, duplicates)
      
      # Remove empty matches
      rem_empty <- which(duplicates$source_entry %in% "" & duplicates$database_entry %in% "")
      if(length(rem_empty) > 0){
        duplicates <- duplicates[-rem_empty,]
      }
      
      # update matched IDs and rows
      matched_rows <- as.integer(subset(duplicates, select=source_row)[,1])
      matched_ids <- as.integer(subset(duplicates, select=noquote(id.field))[,1])
    }
  }
  
  # Fuzzy matching
  for(i in 1:length(fields)){
    # Loop over fields to query
    select <- as.character(subset(source, select = fields[i])[,1])
    
    for(j in 1:length(select)){
      # Loop over individual queries
      res <- GetRecords(select=select[j], field = fields[i], table=table, mode="fuzzy",
                        return.fields = c(id.field, fields[i]), fuzzy.min.sim = fuzzy.min.sims[i])
      
      # Combine results into data.frame
      source_row <- rep(j, nrow(res))
      id <-  as.numeric(subset(res, select=noquote(id.field))[,1])
      duplicate_type <- as.character(rep("similar", nrow(res)))
      similarity <- res$similarity
      field_matched <- rep(fields[i], nrow(res))
      database_entry <- subset(res, select=noquote(fields[i]))[,1]
      source_entry <- rep(select[j], nrow(res))
      dup <- data.frame(source_row, id, duplicate_type, similarity, field_matched, source_entry, database_entry)
      names(dup)[names(dup) %in% "id"] <- id.field
      
      if(all.entries == FALSE){
        # remove records with ID that have already been matched
        dup_ids <- subset(dup, select=id.field)[,1]
        match_master <- which(matched_rows %in% j & matched_ids %in% dup_ids)
        rem_rows <- which(dup$source_row %in% matched_rows[match_master] &
                            dup_ids %in% matched_ids[match_master])
        if(length(rem_rows) > 0){
          dup <- dup[-rem_rows,]
        }
      }
      
      # add results to master duplicate table
      duplicates <- rbind(dup, duplicates)
      
      # Remove empty matches
      rem_empty <- which(duplicates$source_entry %in% "" & duplicates$database_entry %in% "")
      if(length(rem_empty) > 0){
        duplicates <- duplicates[-rem_empty,]
      }
      
      # update matched IDs and rows
      matched_ids <- as.integer(subset(duplicates, select=noquote(id.field))[,1])
      matched_rows <- as.integer(subset(duplicates, select=source_row)[,1])
    }
  }
  
  # Format results
  duplicates_ids <- duplicates[,which(names(duplicates) %in% id.field)]
  ord <- order(duplicates$source_row, as.character(duplicates$duplicate_type),
               -duplicates$similarity, duplicates_ids, duplicates$field_matched)
  duplicates <- duplicates[ord, ]
  rownames(duplicates) <- NULL
  
  if(internal_matching == TRUE){
    # Replace source rows with IDs
    source_rows <- duplicates$source_row
    query_ids <- subset(source, select = id.field)[source_rows, 1]
    source_ids <- GetRecords(select=query_ids, field = id.field, table=table, mode="exact", ids.only = T)
    duplicates$source_row <- source_ids
    names(duplicates)[names(duplicates) =="source_row"] <- paste0(id.field, "_1", collapse="")
    # Change column names
    names(duplicates)[names(duplicates) == id.field] <- paste0(id.field, "_2", collapse="")
    names(duplicates)[names(duplicates) =="source_entry"] <- "database_entry_1"
    names(duplicates)[names(duplicates) =="database_entry"] <- "database_entry_2"
    # Remove matches against self
    rem_self <- which(subset(duplicates,
                             select=paste0(id.field, "_1", collapse=""))[,1]
                      ==
                        subset(duplicates,
                               select=paste0(id.field, "_2", collapse=""))[,1])
    duplicates <- duplicates[-rem_self,]
  }
  
  if(ids.only == TRUE){
    duplicates <- IDsOnly(duplicates, id.field = id.field)
  }
  
  return(duplicates)
}

CheckForDuplicateStudies <- function(studies=NULL,
                                     fields= c("doi", "title", "authors", "abbreviation"),
                                     fuzzy.min.sims=c(1, 0.9, 0.9, 1),
                                     all.entries=FALSE, ids.only=FALSE, conn=eaDB){
  CheckForDuplicates(source=studies, table = "studies", fields=fields,
                     id.field="study_id", fuzzy.min.sims = fuzzy.min.sims,
                     all.entries = all.entries, ids.only = ids.only, conn=conn)
}

CheckForDuplicateAssessors <- function(assessors=NULL,
                                       fields= c("name", "email"),
                                       fuzzy.min.sims=c(0.85, 0.85),
                                       all.entries=FALSE, ids.only=FALSE, conn=eaDB){
  CheckForDuplicates(source=assessors, table = "assessors", fields=fields,
                     id.field="assessor_id", fuzzy.min.sims = fuzzy.min.sims,
                     all.entries = all.entries, ids.only = ids.only, conn=conn)
}

CombineDuplicateStudies <- function(duplicate.ids, original.ids, update.loe=T,
                                    conn=eaDB){
  
  modified <- ReplaceIDs(duplicate.ids = duplicate.ids, original.ids = original.ids,
                         id.field="study_id", sort.field="assessment_id",
                         tables=c("level_of_evidence", "quality"), conn=conn)
  
  studies_removed <- RemoveRecords(ids=duplicate.ids, id.field="study_id",
                                   table="studies", conn=conn)
  
  if(update.loe == TRUE){
    UpdateLoE(study.ids=original.ids, conn=conn)
  }
  
  combined <- list(removed_in_studies = studies_removed,
                   modified_in_level_of_evidence=modified$modified$level_of_evidence,
                   removed_in_level_of_evidence=modified$removed$level_of_evidence,
                   modified_in_quality=modified$modified$quality,
                   removed_in_quality=modified$removed$quality)
  
  return(combined)
}

CombineDuplicateAssessments <- function(duplicate.ids, original.ids, update.loe=T,
                                        conn=eaDB){
  
  modified <- ReplaceIDs(duplicate.ids = duplicate.ids,
                         original.ids = original.ids,
                         id.field="assessment_id", sort.field="study_id",
                         tables=c("level_of_evidence", "quality"),
                         conn=conn)
  
  # Remove from assessments
  assessments_removed <- RemoveRecords(ids=duplicate.ids, id.field="assessment_id",
                                       table="assessments", conn=conn)
  
  
  if(update.loe == TRUE){
    # Get IDs of studies to update
    update_loe_ids <- dbGetQuery(conn=conn,
                                 "SELECT study_id
                                    FROM level_of_evidence
                                   WHERE assessment_id = ?;", params=list(original.ids))
    update_loe_ids <- unique(update_loe_ids[,1])
    UpdateLoE(study.ids=update_loe_ids, conn=conn)
  }
  
  
  combined <- list(removed_in_assessments = assessments_removed,
                   modified_in_level_of_evidence=modified$modified$level_of_evidence,
                   removed_in_level_of_evidence=modified$removed$level_of_evidence,
                   modified_in_quality=modified$modified$quality,
                   removed_in_quality=modified$removed$quality)
  return(combined)
}

CombineDuplicateAssessors <- function(duplicate.ids, original.ids, conn=eaDB){
  modified <- dbExecute(conn=conn, "UPDATE assessments
            SET assessor_id = ?
            WHERE assessor_id = ?;",
                        params=list(original.ids, duplicate.ids))
  removed <- RemoveRecords(ids=duplicate.ids, id.field="assessor_id", table="assessors", conn=conn)
  combined <- list(removed_from_assessors = removed,
                   modified_in_assessments=modified)
  return(combined)
}

###################### ##
# DELETE FUNCTIONS ######
###################### ##

RemoveStudies <- function(study.ids, conn=eaDB){
  n_rem <- RemoveRecords(ids=study.ids, id.field = "study_id", table="studies")
  return(n_rem)
}

RemoveAssessors <- function(assessor.ids, conn=eaDB){
  n_rem <- RemoveRecords(ids=assessor.ids, id.field = "assessor_id", table="assessors")
  return(n_rem)
}

RemoveAssessments <- function(assessment.ids, conn=eaDB){
  n_rem <- RemoveRecords(ids=assessment.ids, id.field = "assessment_id", table="assessments")
  return(n_rem)
}

RemoveEvidenceForStudies <-  function(study.ids, conn=eaDB){
  rem_loe <- RemoveRecords(ids=study.ids,
                           id.field = "study_id",
                           table="level_of_evidence")
  rem_quality <- RemoveRecords(ids=study.ids,
                               id.field = "study_id",
                               table="quality")
  return(list(removed_from_level_of_evidence=rem_loe,
              removed_from_quality=rem_quality))
}

RemoveEvidenceForAssessments <-  function(assessment.ids, conn=eaDB){
  rem_loe <- RemoveRecords(ids=assessment.ids,
                           id.field = "assessment_id",
                           table="level_of_evidence")
  rem_quality <- RemoveRecords(ids=assessment.ids,
                               id.field = "assessment_id",
                               table="quality")
  return(list(removed_from_level_of_evidence=rem_loe,
              removed_from_quality=rem_quality))
}

RemoveEvidence <-  function(record.ids, conn=eaDB){
  
  to_remove <- dbGetQuery(conn=conn, "SELECT assessment_id, study_id
                                        FROM level_of_evidence
                                       WHERE record_id=?;",
                          params=list(record.ids))
  
  rem_loe <- RemoveRecords(ids=record.ids,
                           id.field="record_id",
                           table="level_of_evidence")
  
  q_remove <- paste0("DELETE FROM quality
                            WHERE assessment_id = ?
                              AND study_id = ?;")
  rem_quality <- dbExecute(conn, q_remove, params=list(to_remove$assessment_id,
                                                       to_remove$study_id))
  
  
  return(list(removed_from_level_of_evidence=rem_loe,
              removed_from_quality=rem_quality))
}

###################### ##
# UPDATE FUNCTIONS ######
###################### ##

UpdateStudies <- function(study.ids, studies.update, conn=eaDB){
  # Format input data
  input <- studies.update
  n <- nrow(input)
  study.ids <- as.integer(as.character(study.ids))
  studies <- TemplateStudies(n)
  studies$abbreviation <- as.character(input$abbreviation)
  studies$authors <- as.character(input$authors)
  studies$title <- as.character(input$title)
  studies$year <- as.integer(input$year)
  studies$doi <- as.character(input$doi)
  
  studies_update <- studies
  n_entries <- nrow(studies_update) # number of new entries
  
  if(n_entries > 0){
    # SQL statement for insertion
    update_studies <- dbSendStatement(conn,
                                      "UPDATE studies
                                          SET abbreviation = ?,
                                              authors = ?,
                                              title = ?,
                                              year = ?,
                                              doi = ?
                                        WHERE study_id = ?;")
    dbBind(update_studies, params=list(studies_update$abbreviation,
                                       studies_update$authors,
                                       studies_update$title,
                                       studies_update$year,
                                       studies_update$doi,
                                       study.ids))
    dbClearResult(update_studies)
  }
  updated <- dbGetQuery(conn, "SELECT * FROM studies WHERE study_id = ?;",
                        param=list(study.ids))
  return(updated)
}

UpdateAssessors <- function(assessor.ids, assessors.update, conn=eaDB){
  
  # Format input data
  input <- assessors.update
  n <- nrow(input)
  assessor.ids <- as.integer(as.character(assessor.ids))
  assessors <- TemplateAssessors(n)
  assessors$name <- as.character(input$name)
  assessors$email <- as.character(input$email)
  
  assessors_update <- assessors
  n_entries <- nrow(assessors_update) # number of new entries
  
  if(n_entries > 0){
    # SQL statement for insertion
    update_assessors <- dbSendStatement(conn,
                                        "UPDATE assessors
                                            SET name = ?,
                                                email = ?
                                          WHERE assessor_id = ?;")
    dbBind(update_assessors, params=list(assessors_update$name,
                                         assessors_update$email,
                                         assessor.ids))
    dbClearResult(update_assessors)
  }
  updated <- dbGetQuery(conn, "SELECT * FROM assessors WHERE assessor_id = ?;",
                        param=list(assessor.ids))
  return(updated)
}

UpdateAssessments <- function(assessment.ids, assessment.update, conn=eaDB){
  
  # Format input data
  input <- assessment.update
  n <- nrow(input)
  assessment.ids <- as.integer(as.character(assessment.ids))
  assessments <- TemplateAssessments(n)
  assessments$assessor_id <- as.character(input$assessor_id)
  assessments$source <- as.character(input$source)
  assessments$date_entered <- as.character(input$date_entered)
  
  assessments_update <- assessments
  n_entries <- nrow(assessments_update) # number of new entries
  
  if(n_entries > 0){
    # SQL statement for insertion
    update_assessments <- dbSendStatement(conn,
                                          "UPDATE assessments
                                              SET assessor_id = ?,
                                                  source = ?,
                                                  date_entered = ?
                                            WHERE assessment_id = ?;")
    dbBind(update_assessments, params=list(assessments_update$assessor_id,
                                           assessments_update$source,
                                           assessments_update$date_entered,
                                           assessment.ids))
    dbClearResult(update_assessments)
  }
  updated <- dbGetQuery(conn, "SELECT * FROM assessments WHERE assessor_id = ?;",
                        param=list(assessment.ids))
  return(updated)
}

ReassessStudies <- function(studies, assessment.id, conn=eaDB){
  # Remove existing evidence for studies in given assessment
  to_remove <- expand.grid(study_id=studies$study_id, assessment_id=assessment.id)
  ids_rem <- dbGetQuery(conn, "SELECT record_id
                        FROM level_of_evidence WHERE study_id = ? AND assessment_id = ?;",
                        params=list(to_remove$study_id, to_remove$assessment_id))
  removed <- RemoveEvidence(record.ids=ids_rem$record_id, conn=conn)
  
  # Assess studies
  assessed <- AssessStudies(studies=studies, assessment.id=assessment.id)
  return(assessed)
}

MarkAsReviewed <- function(record.ids, conn=eaDB){
  dbExecute(conn, "UPDATE level_of_evidence
            SET reviewed = 'yes'
            WHERE record_id = ?;", params=list(record.ids))
  reviewed <- GetLoE(select=record.ids, field="record_id", mode="exact", conn=conn)
  return(reviewed)
}

###################### ##
# HELPER FUNCTIONS ######
###################### ##

IDsOnly <- function(results, id.field=NULL){
  # reduces columns of result set to ID columns only
  
  if(is.null(id.field)){
    # If no ID field provided, get columns from data
    id_field_id <- which(grepl("_id", names(results)))
  }
  
  if(!is.null(id.field)){
    # If ID field is provided, get index
    id_field_id <- which(names(results) %in% id.field)
    if(length(id_field_id) == 0){
      # If ID field could not be found, try to guess from data
      id_field_id <- which(grepl("_id", names(results)))
    }
  }
  
  if(length(id_field_id) != 0){
    results <- results[,id_field_id]
  } else {
    warning("No ID fields found in results. All fields will be returned")
  }
  return(results)
}

ReplaceIDs <- function(duplicate.ids, original.ids, id.field, sort.field,
                       tables, conn=eaDB){
  
  if(length(duplicate.ids) != length(original.ids)){
    stop("Numbers of duplicate IDs and original IDs don't match.")
  }
  
  n_ids <- length(duplicate.ids)
  
  # Prepare lists to store number changed and deleted records
  n_removed <- vector("list", length(tables))
  names(n_removed) <- tables
  n_removed[1:length(tables)] <- as.integer(0)
  n_modified <- n_removed
  
  for(i in 1:n_ids){
    duplicate.id <- duplicate.ids[i]
    original.id <- original.ids[i]
    for(j in 1:length(tables)){
      table <- tables[j]
      
      q_sort <- paste0("SELECT ", dbQuoteIdentifier(conn, sort.field),
                       "  FROM ", dbQuoteIdentifier(conn, table),
                       " WHERE ", dbQuoteIdentifier(conn, id.field), " = ?;")
      q_delete <- paste0("DELETE FROM ", dbQuoteIdentifier(conn, table),
                         "      WHERE ", dbQuoteIdentifier(conn, sort.field), " = ?
                                  AND ", dbQuoteIdentifier(conn, id.field), " = ?;")
      q_update <- paste0("UPDATE ",  dbQuoteIdentifier(conn, table),
                         "   SET ", dbQuoteIdentifier(conn, id.field), " = ?
                           WHERE ", dbQuoteIdentifier(conn, id.field), " = ?;")
      
      # Get assessment_ids for duplicate and original
      sort_ids_dup <- dbGetQuery(conn, q_sort, params=list(duplicate.id))
      sort_ids_org <- dbGetQuery(conn, q_sort, params=list(original.id))
      sort_ids_dup <- unique(sort_ids_dup[,1])
      sort_ids_org <- unique(sort_ids_org[,1])
      
      # Finding assignments for duplicate that already contain the original
      sort_ids_dup_del <- sort_ids_org[which(sort_ids_org %in% sort_ids_dup)]
      
      # Delete duplicate study from assignments that already contain original
      records_delete <- list(sort_ids_dup_del,
                             rep(duplicate.id, length(sort_ids_dup_del)))
      n_del_new <- dbExecute(conn, q_delete, params=records_delete)
      
      # Replace duplicate binding with original
      records_replace <- list(as.character(original.id), as.character(duplicate.id))
      n_repl_new <- dbExecute(conn, q_update, params=records_replace)
      # using dbBind() instead of dbExecute crashes R if there are no rows to replace!
      n_removed[j] <- as.integer(n_removed[j]) + as.integer(n_del_new)
      n_modified[j] <- as.integer(n_modified[j]) + as.integer(n_repl_new)
    }
  }
  return(list(modified=n_modified, removed=n_removed))
}

RemoveRecords <- function(ids, id.field, table, conn=eaDB){
  q_remove <- paste0("DELETE FROM ", dbQuoteIdentifier(conn, table),
                     "WHERE ", dbQuoteIdentifier(conn, id.field), " = ?;")
  n_rem <- dbExecute(conn, q_remove, params=list(ids))
  return(n_rem)
}

UpdateLoE <- function(study.ids=NULL, conn=eaDB){
  if(is.null(study.ids)){
    study.ids <- GetStudies(ids.only = T)
  }
  
  # Get info on IDs
  studies <- dbGetQuery(conn=conn,
                        "SELECT study_id, assessment_id, study_design
                        FROM level_of_evidence
                        WHERE study_id = ?;",
                        params=list(study.ids))
  
  # Get preliminary LoE
  loe_pre <- dbGetQuery(conn, "SELECT loe_pre
                        FROM study_designs
                        WHERE study_design = ?;",
                        param=list(studies$study_design))
  
  # Check whether a preliminary LoE could be assigned to each study
  if(nrow(loe_pre) != nrow(studies)){
    designs <- dbGetQuery(conn, "SELECT DISTINCT study_design FROM study_designs;")
    stop(paste0("One or more study designs could not been assigned a preliminary LoE. Please assure that the study design provided for each study is one of the following: '", paste(designs$study_design, collapse = "', '","'"), sep = ""))
  }
  
  # Add assigned preliminary LoE
  studies$loe_pre <- loe_pre$loe_pre
  
  # Calculate quality scores based on existing information in quality table;
  # calculations are performed at the database level
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
  studies$q_score[studies$q_score == -9] <- NA  # Reset to NA
  
  # Update values in LoE
  q_update <- paste0("UPDATE level_of_evidence
                     SET loe_final = ?,
                     loe_pre = ?,
                     points_p = ?,
                     points_q = ?,
                     q_score = ?,
                     downgrading = ?
                     WHERE assessment_id = ?
                     AND study_id = ?;")
  records_update <- list(studies$loe_final,
                         studies$loe_pre,
                         as.integer(studies$points_p),
                         as.integer(studies$points_q),
                         studies$q_score,
                         studies$downgrading,
                         studies$assessment_id,
                         studies$study_id)
  
  loe_update <- dbSendStatement(conn,q_update)
  dbBind(loe_update, params=records_update)
  dbClearResult(loe_update)
  
  updated_records <- dbGetQuery(conn, "SELECT assessment_id,
                                study_id,
                                loe_final,
                                loe_pre,
                                points_p,
                                points_q,
                                q_score,
                                downgrading
                                FROM level_of_evidence
                                WHERE assessment_id = ?
                                AND study_id = ?;",
                                params=list(studies$assessment_id, studies$study_id))
  ord <- order(updated_records$assessment_id, updated_records$study_id)
  return(updated_records[ord,])
}
