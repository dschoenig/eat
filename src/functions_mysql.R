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

AssessStudies <- function(studies, assessment_id, conn=eaDB){
  # TODO: check whether combination of assessment and study ids is already present in LoE table
  
  # Get info on checklist
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
                                     as.integer(studies$points_p), # remove?
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
  if(is.null(select) && is.null(field)){
    # Return entire table if no query is entered; else perform query
    query <- paste0("SELECT * FROM ", 
                    dbQuoteIdentifier(conn, table), ";")
    results <- dbGetQuery(conn, query)
  } else {
    # Perform query depending on query mode
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
     
    # Query table
    if(mode=="similar"){
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
      }
      
      if(!is.null(return.fields)){
        results <- subset(results, select=c("distance", return.fields))
      }
    }
  }
  
  if(ids.only == TRUE){
    # Reduce columns of result set to id columns only
    id_col <- which(grepl("_id", names(results)))
    if(length(id_col) != 0){
      results <- results[,id_col]
    } else {
      warning("No ID columns found in results. All columns will be included.")
    }
  }
  return(results)
}

GetStudies <- function(select=NULL, field=NULL, ids.only = FALSE,
                       mode="similar", fuzzy.th=0.25, conn=eaDB){
  
  results <- GetRecords(select=select, field=field, ids.only = ids.only,
                        table="studies", mode=mode, 
                        fuzzy.th=fuzzy.th, conn=conn)
  return(results)
}

GetAssessors <- function(select=NULL, field=NULL, ids.only = FALSE,
                         mode="similar", fuzzy.th=0.25, conn=eaDB){
  
  results <- GetRecords(select=select, field=field, ids.only = ids.only,
                        table="assessors", mode=mode, fuzzy.th=fuzzy.th, 
                        conn=conn)
  return(results)
  
}

GetAssessments <- function(select=NULL, field=NULL, ids.only = FALSE,
                           mode="similar", fuzzy.th=0.25, conn=eaDB){
  
  results <- GetRecords(select=select, field=field, ids.only = ids.only, 
                        table="assessments", mode=mode, fuzzy.th=fuzzy.th, 
                        conn=conn)
  
  return(results)
}

GetLoE <- function(select=NULL, field=NULL, ids.only = FALSE,
                   mode="similar", fuzzy.th=0.25, conn=eaDB){
  
  results <- GetRecords(select=select, field=field, ids.only = ids.only, 
                        table="level_of_evidence", mode=mode, 
                        fuzzy.th=fuzzy.th, conn=conn)
  
  return(results)
}

GetFullRecords <- function(select=NULL, field=NULL, ids.only = FALSE,
                           mode="similar", fuzzy.th=0.25, conn=eaDB){
  
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
    if(mode == "similar"){
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
    }
  }
  
  if(ids.only == TRUE){
    # Reduce columns of result set to id columns only
    id_col <- which(grepl("_id", names(results)))
    if(length(id_col) != 0){
      results <- results[,id_col]
    } else {
      warning("No ID columns found in results. All columns will be included.")
    }
  }
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
