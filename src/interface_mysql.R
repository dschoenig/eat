# These functions are built to interact with a database for evidence assessment,
# set up according to Mupepele et al. (2016). The functions serve as building
# blocks for a CRUD interface and cover data entry, retrieval, updating and
# removal, as well as duplicate handling.
#
# The R interface depends on a correctly structured and initialized MySQL or
# MariaDB database. Please refer to the SQL documentation for the evidence
# assessment database to ensure that your database has been set up correctly.
# Some functions will not work for database users with restricted rights. In
# particular, all `CombineDuplicates` functions, all `Remove` functions, and
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

######################################################################### #
# DEPENDENCIES#############################################################
######################################################################### #

# Check for installed packages and install, if necessary
if(!all(c("DBI", "RMariaDB", "stringdist", "tidyr") %in% rownames(installed.packages()))){
  to_install <- !c("DBI", "RMariaDB", "stringdist", "tidyr") %in% rownames(installed.packages())
  packages_install <- c("DBI", "RMariaDB", "stringdist", "tidyr")[to_install]
  install.packages(packages_install)
}

# Load packages
library(DBI)
library(stringdist)
library(tidyr)

######################################################################### #
# DATA ENTRY ##############################################################
######################################################################### #

CreateStudies <- function(studies, force=FALSE, conn=eaDB){
  # Creates new entries in the `studies` table.
  #
  # Args:
  #   studies: A dataframe in the format provded by `TemplateStudies()`
  #   force: If FALSE prevents entry of potential duplicates. If TRUE, forces
  #     data entry even if duplicates have been detected. Default is FALSE.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame containing new entries in the `studies` table.

  # Format input data
  input <- studies
  n <- nrow(input)
  studies <- TemplateStudies(n)
  studies$abbreviation <- as.character(input$abbreviation)
  studies$authors <- as.character(input$authors)
  studies$title <- as.character(input$title)
  studies$year <- as.integer(as.character(input$year))
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
  # Creates new entries in the `assessors` table.
  #
  # Args:
  #   assessors: A dataframe in the format provded by `TemplateAssessors()`
  #   force: If FALSE prevents entry of potential duplicates. If TRUE, forces
  #     data entry even if duplicates have been detected. Default is FALSE.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame containing new entries in the `assessors` table.

  # Format input data
  input <- assessors
  n <- nrow(input)
  assessors <- TemplateAssessors(N=n)
  assessors$name <- as.character(input$name)
  assessors$affiliation <- as.character(input$affiliation)
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
                                       "INSERT INTO assessors(name, affiliation, email)
                                             VALUES (?, ?, ?);")
    dbBind(insert_assessor, param=list(assessors_new$name,
                                       assessors_new$affiliation,
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

CreateAssessments <- function(assessments, conn=eaDB){
  # Creates new entries in the `assessments` table.
  #
  # Args:
  #   assessments: A dataframe in the format provded by `TemplateAssessments()`
  #   force: If FALSE prevents entry of potential duplicates. If TRUE, forces
  #     data entry even if duplicates have been detected. Default is FALSE.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame containing new entries in the `assessments` table.

  # If date is not specified, set today's date
  today <- Sys.Date()
  empty <- which(assessments$date_entered == "")
  assessments$date_entered[empty] <- as.character(today)

  # Format input data
  input <- assessments
  n <- nrow(input)
  assessments <- TemplateAssessments(n)
  assessments$assessor_id <- as.integer(as.character(input$assessor_id))
  assessments$source <- as.character(input$source)
  assessments$date_entered <- as.character(input$date_entered)

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
  # Assesses the level of evidence for the studies provided and creates new
  # entries in the `level_of_evidence` table.
  #
  # Args:
  #   studies: A data frame in the format provded by `TemplateAssessStudies()`
  #   assessment.id: A valid entry in the field `assessment_id` of the
  #     `assessments` table.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame containing new entries in the `level_of_evidence` table.

  # Get info on checklist
  n_questions <- as.integer(dbGetQuery(conn, "SELECT COUNT(question_id) FROM checklist")[1,1])
  c_questions <- paste0("q",seq(1:n_questions))

  # Format input data
  input <- studies
  n <- nrow(input)
  studies <- TemplateAssessStudies(n)
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

######################################################################### #
# DATA RETRIEVAL ##########################################################
######################################################################### #

GetRecords <- function(query=NULL, field=NULL, table, return.fields=NULL,
                       ids.only=FALSE, mode="exact", fuzzy.min.sim=0.7,
                       conn=eaDB) {
  # Retrieves records from tables in the evidence assessment database.
  #
  # Args:
  #   query: Query term for the field to be queried. A character vector of
  #     several query terms can be used for modes `exact` and `partial`. If
  #     NULL, the entire table will be returned. Default is NULL.
  #   field: Field to be queried in table. If NULL, the entire table will be
  #     returned. Default is NULL.
  #   table: Table to be queried.
  #   return.fields: Fields to be included in the returned data frame. If NULL,
  #     all fields will be returned. Default is NULL.
  #   ids.only: If TRUE, only fields that contain IDs are returned. If FALSE,
  #     fields to be returned will depend on `return.fields`. Default is FALSE.
  #   mode: Offers three modes for matching the query term; "exact" will only
  #     return exact matches to the query term; "partial" will also include
  #     partial matches; "fuzzy" matches similar terms based on a minimum
  #     similarity set by `fuzzy.min.sim`. Default is "exact".
  #   fuzzy.min.sim: Minimum similarity for `mode="fuzzy"`. Ranges from 0
  #     (everything matches) to 1 (only exact matches). Default is 0.7.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of matching records in the specified table.

  qterm <- query # rename
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
  if(is.null(qterm) && !is.null(field)){
    warning(paste0("No query term provided for field '", field,
                   "'. Entire table will be returned."))
    field  <-  NULL
  }
  # If query term is provided but no field name, print warning that entire table
  # will be returned
  if(!is.null(qterm) && is.null(field)){
    warning(paste0("No field name provided for query term '", qterm,
                   "'. Entire table will be returned."))
    qterm  <-  NULL
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

  if(is.null(qterm) && is.null(field)){
    # Return entire table if no query is entered; else perform query
    query <- paste0("SELECT ",
                    columns,
                    " FROM ",
                    dbQuoteIdentifier(conn, table), ";")
    results <- dbGetQuery(conn, query)
  } else {
    # Perform query depending on query mode
    if(mode=="partial"){
      qterm <- paste("%", as.character(qterm), "%", sep = "")
      query <- paste0("SELECT ",
                      columns,
                      " FROM ",
                      dbQuoteIdentifier(conn, table),
                      " WHERE ",
                      dbQuoteIdentifier(conn, field),
                      " LIKE ?;")
      results <- dbGetQuery(conn, query, params=list(qterm))
    }

    if(mode == "exact"){
      query <- paste0("SELECT ",
                      columns,
                      " FROM ",
                      dbQuoteIdentifier(conn, table),
                      " WHERE ",
                      dbQuoteIdentifier(conn, field),
                      " = ?;")
      results <- dbGetQuery(conn, query, params=list(qterm))
    }

    if(mode == "fuzzy"){
      if(length(qterm) > 1){
        stop("For fuzzy matching, please provide only one query term.")
      }
      fuzzy.th <- 1 - fuzzy.min.sim
      query <- paste0("SELECT * FROM ",
                      dbQuoteIdentifier(conn, table))
      res <- dbSendQuery(conn, query)
      res_part <- dbFetch(res, n=50)
      if(nrow(res_part) > 0) {
        res_part_q <- subset(res_part, select=noquote(field))[,1]
        dist_jw <- stringdist(qterm, res_part_q, method="jw", p=0.1)
        res_part <- cbind(distance=round(dist_jw, 2), res_part)
        results <- subset(res_part, distance <= fuzzy.th)
        repeat {
          res_part <- dbFetch(res, n=2)
          if(nrow(res_part) <= 1) {
            break
          }
          res_part_q <- subset(res_part, select=noquote(field))[,1]
          dist_jw <- stringdist(qterm, res_part_q, method="jw", p=0.1)
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

GetStudies <- function(query=NULL, field=NULL, return.fields=NULL,
                       ids.only=FALSE, mode="exact", fuzzy.min.sim=0.7,
                       conn=eaDB){
  # Retrieves records from the `studies` table in the evidence assessment
  # database.
  #
  # Args:
  #   query: Query term for the field to be queried. A character vector of
  #     several query terms can be used for modes `exact` and `partial`. If
  #     NULL, the entire table will be returned. Default is NULL.
  #   field: Field to be queried in table. If NULL, the entire table will be
  #     returned. Default is NULL.
  #   return.fields: Fields to be included in the returned data frame. If NULL,
  #     all fields will be returned. Default is NULL.
  #   ids.only: If TRUE, only fields that contain IDs are returned. If FALSE,
  #     fields to be returned will depend on `return.fields`. Default is FALSE.
  #   mode: Offers three modes for matching the query term; "exact" will only
  #     return exact matches to the query term; "partial" will also include
  #     partial matches; "fuzzy" matches similar terms based on a minimum
  #     similarity set by `fuzzy.min.sim`. Default is "exact".
  #   fuzzy.min.sim: Minimum similarity for `mode="fuzzy"`. Ranges from 0
  #     (everything matches) to 1 (only exact matches). Default is 0.7.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of matching records in the `studies` table.

  results <- GetRecords(query=query, field=field, table="studies",
                        return.fields=return.fields, ids.only = ids.only,
                        mode=mode, fuzzy.min.sim=fuzzy.min.sim, conn=conn)
  return(results)
}

GetAssessors <- function(query=NULL, field=NULL, return.fields=NULL,
                         ids.only=FALSE, mode="exact", fuzzy.min.sim=0.7,
                         conn=eaDB){
  # Retrieves records from the `assessors` table in the evidence assessment
  # database.
  #
  # Args:
  #   query: Query term for the field to be queried. A character vector of
  #     several query terms can be used for modes `exact` and `partial`. If
  #     NULL, the entire table will be returned. Default is NULL.
  #   field: Field to be queried in table. If NULL, the entire table will be
  #     returned. Default is NULL.
  #   return.fields: Fields to be included in the returned data frame. If NULL,
  #     all fields will be returned. Default is NULL.
  #   ids.only: If TRUE, only fields that contain IDs are returned. If FALSE,
  #     fields to be returned will depend on `return.fields`. Default is FALSE.
  #   mode: Offers three modes for matching the query term; "exact" will only
  #     return exact matches to the query term; "partial" will also include
  #     partial matches; "fuzzy" matches similar terms based on a minimum
  #     similarity set by `fuzzy.min.sim`. Default is "exact".
  #   fuzzy.min.sim: Minimum similarity for `mode="fuzzy"`. Ranges from 0
  #     (everything matches) to 1 (only exact matches). Default is 0.7.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of matching records in the `assessors` table.
  results <- GetRecords(query=query, field=field, table="assessors",
                        return.fields=return.fields, ids.only = ids.only,
                        mode=mode, fuzzy.min.sim=fuzzy.min.sim,
                        conn=conn)
  return(results)
}

GetAssessments <- function(query=NULL, field=NULL, return.fields=NULL,
                           ids.only=FALSE, mode="exact", fuzzy.min.sim=0.7,
                           conn=eaDB){
  # Retrieves records from the `assessments` table in the evidence assessment
  # database.
  #
  # Args:
  #   query: Query term for the field to be queried. A character vector of
  #     several query terms can be used for modes `exact` and `partial`. If
  #     NULL, the entire table will be returned. Default is NULL.
  #   field: Field to be queried in table. If NULL, the entire table will be
  #     returned. Default is NULL.
  #   return.fields: Fields to be included in the returned data frame. If NULL,
  #     all fields will be returned. Default is NULL.
  #   ids.only: If TRUE, only fields that contain IDs are returned. If FALSE,
  #     fields to be returned will depend on `return.fields`. Default is FALSE.
  #   mode: Offers three modes for matching the query term; "exact" will only
  #     return exact matches to the query term; "partial" will also include
  #     partial matches; "fuzzy" matches similar terms based on a minimum
  #     similarity set by `fuzzy.min.sim`. Default is "exact".
  #   fuzzy.min.sim: Minimum similarity for `mode="fuzzy"`. Ranges from 0
  #     (everything matches) to 1 (only exact matches). Default is 0.7.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of matching records in the `assessments` table.

  results <- GetRecords(query=query, field=field, table="assessments",
                        return.fields=return.fields, ids.only = ids.only,
                        mode=mode, fuzzy.min.sim=fuzzy.min.sim, conn=conn)

  return(results)
}

GetLoE <- function(query=NULL, field=NULL, return.fields=NULL, ids.only=FALSE,
                   mode="exact", fuzzy.min.sim=0.7, conn=eaDB){
  # Retrieves records from the `level_of_evidence` table in the evidence
  # assessment database.
  #
  # Args:
  #   query: Query term for the field to be queried. A character vector of
  #     several query terms can be used for modes `exact` and `partial`. If
  #     NULL, the entire table will be returned. Default is NULL.
  #   field: Field to be queried in table. If NULL, the entire table will be
  #     returned. Default is NULL.
  #   return.fields: Fields to be included in the returned data frame. If NULL,
  #     all fields will be returned. Default is NULL.
  #   ids.only: If TRUE, only fields that contain IDs are returned. If FALSE,
  #     fields to be returned will depend on `return.fields`. Default is FALSE.
  #   mode: Offers three modes for matching the query term; "exact" will only
  #     return exact matches to the query term; "partial" will also include
  #     partial matches; "fuzzy" matches similar terms based on a minimum
  #     similarity set by `fuzzy.min.sim`. Default is "exact".
  #   fuzzy.min.sim: Minimum similarity for `mode="fuzzy"`. Ranges from 0
  #     (everything matches) to 1 (only exact matches). Default is 0.7.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of matching records in the `level_of_evidence` table.

  results <- GetRecords(query=query, field=field, table="level_of_evidence",
                        return.fields=return.fields, ids.only = ids.only,
                        mode=mode, fuzzy.min.sim=fuzzy.min.sim,   conn=conn)

  return(results)
}

GetFullRecords <- function(query=NULL, field=NULL, ids.only = FALSE,
                           mode="exact", fuzzy.min.sim=0.7, conn=eaDB){
  # Retrieves full records from the database, joining entries of the `studies`,
  # `level_of_evidence`, `assessments` and `assessors` tables.
  #
  # Args:
  #   query: Query term for the field to be queried. A character vector of
  #     several query terms can be used for modes `exact` and `partial`. If
  #     NULL, the entire table will be returned. Default is NULL.
  #   field: Field to be queried. Any field of the tables `studies`,
  #     `level_of_evidence`, `assessments` and `assessors` is permitted. If
  #     NULL, the entire table will be returned. Default is NULL.
  #   ids.only: If TRUE, only fields that contain IDs are returned. If FALSE,
  #     fields to be returned will depend on `return.fields`. Default is FALSE.
  #   mode: Offers three modes for matching the query term; "exact" will only
  #     return exact matches to the query term; "partial" will also include
  #     partial matches; "fuzzy" matches similar terms based on a minimum
  #     similarity set by `fuzzy.min.sim`. Default is "exact".
  #   fuzzy.min.sim: Minimum similarity for `mode="fuzzy"`. Ranges from 0
  #     (everything matches) to 1 (only exact matches). Default is 0.7.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of matching records in the database.

  qterm <- query # rename
  # If query term is provided but no field name, warn that entire table will be
  # returned
  if(!is.null(qterm) && is.null(field)){
    warning(paste0("No field name provided for query term '", qterm,
                   "'. Entire table will be returned."))
    qterm  <-  NULL
  }

  # Return entire table if no query is entered; else perform query
  if(is.null(qterm) && is.null(field)){
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
      qterm <- paste("%", as.character(qterm), "%", sep = "")
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
      results <- dbGetQuery(conn, query, params=list(qterm))
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
      results <- dbGetQuery(conn, query, params=list(qterm))
    }
    if(mode == "fuzzy"){
      if(length(qterm) > 1){
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
        dist_jw <- stringdist(qterm, res_part_q, method="jw", p=0.1)
        res_part <- cbind(distance=round(dist_jw, 2), res_part)
        results <- subset(res_part, distance <= fuzzy.th)
        repeat {
          res_part <- dbFetch(res, n=2)
          if(nrow(res_part) <= 1) {
            break
          }
          res_part_q <- subset(res_part, select=noquote(field))[,1]
          dist_jw <- stringdist(qterm, res_part_q, method="jw", p=0.1)
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
  # Retrieves entries for studies in the `studies` table that have not yet been
  # assessed.
  #
  # Args:
  #   ids.only: If TRUE, only fields that contain IDs are returned. If FALSE,
  #     all fields will be returned. Default is FALSE.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #  A data frame of containing entries of studies for which no assessment
  #  information exists in the database.


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
    not_assessed_studies <- GetStudies(query=not_assessed, field="study_id",
                                       mode="exact", conn=conn)
    return(not_assessed_studies)
  }
}

GetRecordsToReview <- function(ids.only=FALSE, conn=eaDB){
  # Retrieves full records that are not marked as reviewed.
  #
  # Args:
  #   ids.only: If TRUE, only fields that contain IDs are returned. If FALSE,
  #     all fields will be returned. Default is FALSE.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #  A data frame of containing full records not marked as reviewed.

  to_review <- dbGetQuery(conn, "SELECT record_id FROM level_of_evidence WHERE reviewed = 'no';")
  to_review <- to_review[,1]
  if(ids.only == TRUE){
    return(to_review)
  } else {
    to_review_records <- GetFullRecords(query=to_review, field="record_id",
                                        mode="exact", conn=conn)
    return(to_review_records)
  }
}

######################################################################### #
# DUPLICATE HANDLING ######################################################
######################################################################### #

CheckForDuplicates <- function(source=NULL, table, fields, id.field,
                               fuzzy.min.sims,
                               all.entries=FALSE, ids.only=FALSE, conn=eaDB){
  # Checks whether entries in the provided source data frame are already
  # present in database. If no source is provided, checks for duplicates
  # present within the database. This is a general purpose function. Specific
  # functions are provided for detecting duplicates in the `studies` and
  # `assessor` tables.
  #
  # Args:
  #   source: A data frame corresponding to the format of the table to be
  #     checked for duplicates. Use the `Template` functions for examples.
  #     If NULL, entries of the database table will be matched against
  #     themselves to detect duplicates within the database. Default is NULL.
  #   table: Table to be checked for duplicates.
  #   fields: Fields in table to be used for duplicate detection.
  #   id.field: Name of field that holds the unique IDs in the table.
  #   fuzzy.min.sims: A numeric vector containing the minimum similarities for
  #     duplicate matching based on similarity. The vector must contain one
  #     entry for each field provided in `fields`, in the same order. Values
  #     can range from 0 (everything matches) to 1 (only exact matches).
  #   all.entries: If FALSE, will not include duplicates based on partial or
  #     similarity matching, if a corresponding exact duplicate has already
  #     been detected. In the same way, duplicates based on similarity matching
  #     will not be included if a corresponding partial duplicate has already
  #     been detected. If TRUE, no duplicate matches will be excluded. Default
  #     is FALSE.
  #   ids.only: If TRUE, only fields that contain IDs are returned. If FALSE,
  #     all fields will be returned. Default is FALSE.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame containing duplicates, including the type of duplicates, the
  #   corresponding ID in the database and corresponding row in the provided
  #   data frame. If no source is provided and an internal matching performed,
  #   the returned data frame includes two IDs columns.

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
      res <- GetRecords(query=select[j], field = fields[i], table=table, mode="exact",
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
      res <- GetRecords(query=select[j], field = fields[i], table=table, mode="fuzzy",
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
    source_ids <- GetRecords(query=query_ids, field = id.field, table=table, mode="exact", ids.only = T)
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
                                     fields=c("doi", "title", "authors",
                                              "abbreviation"),
                                     fuzzy.min.sims=c(1, 0.9, 0.9, 1),
                                     all.entries=FALSE, ids.only=FALSE,
                                     conn=eaDB){
  # Checks whether entries in the provided `studies` data frame are already
  # present in the `studies` table. If no data frame is provided, checks for
  # duplicates present within the `studies` table.
  #
  # Args:
  #   source: A data frame as used for entry of new studies with
  #     `CreateStudies()` and produced with `TemplateStudies()`.
  #   fields: Fields in table to be used for duplicate detection. Default is
  #     `c("doi", "title", "authors", "abbreviation")`.
  #   fuzzy.min.sims: A numeric vector containing the minimum similarities for
  #     duplicate matching based on similarity. The vector must contain one
  #     entry for each field provided in `fields`, in the same order. Values
  #     can range from 0 (everything matches) to 1 (only exact matches).
  #     Default is `c(1, 0.9, 0.9, 1)``.
  #   all.entries: If FALSE, will not include duplicates based on partial or
  #     similarity matching, if a corresponding exact duplicate has already
  #     been detected. In the same way, duplicates based on similarity matching
  #     will not be included if a corresponding partial duplicate has already
  #     been detected. If TRUE, no duplicate matches will be excluded. Default
  #     is FALSE.
  #   ids.only: If TRUE, only fields that contain IDs are returned. If FALSE,
  #     all fields will be returned. Default is FALSE.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame containing duplicates, including the type of duplicates, the
  #   corresponding ID in the `studies` table and corresponding row in the
  #   provided data frame. If no source is provided and an internal matching
  #   performed, the returned data frame includes two IDs columns corresponding
  #   to the `study_id` field.

  CheckForDuplicates(source=studies, table = "studies", fields=fields,
                     id.field="study_id", fuzzy.min.sims = fuzzy.min.sims,
                     all.entries = all.entries, ids.only = ids.only, conn=conn)
}

CheckForDuplicateAssessors <- function(assessors=NULL,
                                       fields= c("name", "email"),
                                       fuzzy.min.sims=c(0.85, 0.85),
                                       all.entries=FALSE, ids.only=FALSE,
                                       conn=eaDB){
  # Checks whether entries in the provided `assessors` data frame are already
  # present in the `assessors` table. If no data frame is provided, checks for
  # duplicates present within the `studies` table.
  #
  # Args:
  #   source: A data frame as used for entry of new assessors with
  #     `CreateAssessors()` and produced with `TemplateAssessors()`.
  #   fields: Fields in table to be used for duplicate detection. Default is
  #     `c("name", "email")`.
  #   fuzzy.min.sims: A numeric vector containing the minimum similarities for
  #     duplicate matching based on similarity. The vector must contain one
  #     entry for each field provided in `fields`, in the same order. Values
  #     can range from 0 (everything matches) to 1 (only exact matches).
  #     Default is `c(0.85, 0.85)``.
  #   all.entries: If FALSE, will not include duplicates based on partial or
  #     similarity matching, if a corresponding exact duplicate has already
  #     been detected. In the same way, duplicates based on similarity matching
  #     will not be included if a corresponding partial duplicate has already
  #     been detected. If TRUE, no duplicate matches will be excluded. Default
  #     is FALSE.
  #   ids.only: If TRUE, only fields that contain IDs are returned. If FALSE,
  #     all fields will be returned. Default is FALSE.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame containing duplicates, including the type of duplicates, the
  #   corresponding ID in the `assessors` table and corresponding row in the
  #   provided data frame. If no source is provided and an internal matching
  #   performed, the returned data frame includes two IDs columns corresponding
  #   to the `assessor_id` field.


  CheckForDuplicates(source=assessors, table = "assessors", fields=fields,
                     id.field="assessor_id", fuzzy.min.sims = fuzzy.min.sims,
                     all.entries = all.entries, ids.only = ids.only, conn=conn)
}

CombineDuplicateStudies <- function(duplicate.ids, original.ids,
                                    update.loe=TRUE, conn=eaDB){
  # Combine duplicate studies for the entire database.
  #
  # Args:
  #   duplicate.ids: A numeric or integer vector containing the IDs of
  #     duplicate studies (referring to entries in the `study_id` field), in the
  #     same order as the IDs of the corresponding originals.
  #   original.ids: A numeric or integer vector containing the IDs of the
  #     original studies (referring to entries in the `study_id` field), in the
  #     same order as the IDs of the corresponding duplicates.
  #   update.loe: If TRUE, the quality scores and level of evidence in the
  #     `level_of_evidence` table will be calculated for each original study.
  #     If FALSE, not. Updating can be done seperately by using `UpdateLoE()`.
  #     Default is TRUE.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A list indicating the number of removed and modified entries in the
  #   tables `studies`, `level_of_evidence` and `quality`.

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

CombineDuplicateAssessments <- function(duplicate.ids, original.ids,
                                        update.loe=TRUE, conn=eaDB){
  # Combine duplicate assessments for the entire database.
  #
  # Args:
  #   duplicate.ids: A numeric or integer vector containing the IDs of
  #     duplicate assessments (referring to entries in the `assessments_id`
  #     field), in the same order as the IDs of the corresponding original
  #     entries.
  #   original.ids: A numeric or integer vector containing the IDs of the
  #     original assessments (referring to entries in the `assessments_id`
  #     field), in the same order as the IDs of the corresponding duplicates.
  #   update.loe: If TRUE, the quality scores and level of evidence in the
  #     `level_of_evidence` table will be calculated for each original
  #     assessment. If FALSE, not. Updating can be done seperately by using
  #     `UpdateLoE()`. Default is TRUE.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A list indicating the number of removed and modified entries in the
  #   tables `assessments`, `level_of_evidence` and `quality`.

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
  # Combine duplicate assessors for the entire database.
  #
  # Args:
  #   duplicate.ids: A numeric or integer vector containing the IDs of
  #     duplicate assessors (referring to entries in the `assessor_id`
  #     field), in the same order as the IDs of the corresponding original
  #     entries.
  #   original.ids: A numeric or integer vector containing the IDs of the
  #     original assessors (referring to entries in the `assessor_id`
  #     field), in the same order as the IDs of the corresponding duplicates.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A list with the numbers of removed and modified entries in thetables
  #   `assessors` and `assessments`.

  modified <- dbExecute(conn=conn, "UPDATE assessments
            SET assessor_id = ?
            WHERE assessor_id = ?;",
                        params=list(original.ids, duplicate.ids))
  removed <- RemoveRecords(ids=duplicate.ids, id.field="assessor_id", table="assessors", conn=conn)
  combined <- list(removed_from_assessors = removed,
                   modified_in_assessments=modified)
  return(combined)
}

######################################################################### #
# UPDATE FUNCTIONS ########################################################
######################################################################### #

UpdateStudies <- function(study.ids, studies.update, conn=eaDB){
  # Updates entire entries in the `studies` table.
  #
  # Args:
  #   study.ids: A vector of study IDs to be updated (refers to entries in the
  #     `study_id` field in the `studies` table). Must be provided in the same
  #     order as corresponding entries in `studies.update`.
  #   studies.update: A data frame with updated information in the format
  #     provided by `TemplateStudies()`. Must be provided in the same order as
  #     the corresponding IDs in `study.ids`.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of updated records in the `studies` table.

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
  # Updates entire entries in the `assessors` table.
  #
  # Args:
  #   assessor.ids: A vector of assessor IDs to be updated (refers to entries
  #     in the `assessor_id` field in the `assessors` table). Must be provided
  #     in the same order as corresponding entries in `assessors.update`.
  #   assessors.update: A data frame with updated information in the format
  #     provided by `TemplateAssessors()`. Must be provided in the same order
  #     as the corresponding IDs in `assessor.ids`.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of updated records in the `assessors` table.

  # Format input data
  input <- assessors.update
  n <- nrow(input)
  assessor.ids <- as.integer(as.character(assessor.ids))
  assessors <- TemplateAssessors(n)
  assessors$name <- as.character(input$name)
  assessors$affiliation <- as.character(input$affiliation)
  assessors$email <- as.character(input$email)

  assessors_update <- assessors
  n_entries <- nrow(assessors_update) # number of new entries

  if(n_entries > 0){
    # SQL statement for insertion
    update_assessors <- dbSendStatement(conn,
                                        "UPDATE assessors
                                        SET name = ?,
                                            affiliation = ?,
                                            email = ?
                                        WHERE assessor_id = ?;")
    dbBind(update_assessors, params=list(assessors_update$name,
                                         assessors_update$affiliation,
                                         assessors_update$email,
                                         assessor.ids))
    dbClearResult(update_assessors)
  }
  updated <- dbGetQuery(conn, "SELECT * FROM assessors WHERE assessor_id = ?;",
                        param=list(assessor.ids))
  return(updated)
}

UpdateAssessments <- function(assessment.ids, assessments.update, conn=eaDB){
  # Updates entire entries in the `assessments` table.
  #
  # Args:
  #   assessment.ids: A vector of assessment IDs to be updated (refers to
  #     entries in the `assessment_id` field in the `assessments` table). Must
  #     be provided in the same order as corresponding entries in
  #     `assessments.update`.
  #   assessments.update: A data frame with updated information in the format
  #     provided by `TemplateAssessors()`. Must be provided in the same order
  #     as the corresponding IDs in `assessment.ids`.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of updated records in the `assessments` table.

  # If date is not specified, set today's date
  today <- Sys.Date()
  empty <- which(input$date_entered == "")
  assessments.update$date_entered[empty] <- as.character(today)

  # Format input data
  input <- assessments.update
  n <- nrow(input)
  assessments <- TemplateAssessments(n)
  assessments$assessor_id <- as.integer(as.character(input$assessor_id))
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
  # Removes existing quality assessment data for specific studies that are part
  # of a specific assessment, and then reassesses studies based on updated
  # information. For a simple update of the quality scores and level of
  # evidence based on quality assessment data that is already present in the
  # database, use `UpdateLoE()`
  #
  # Args:
  #   studies: A data frame with updated quality information, in the format
  #     provided by `TemplateAssessStudies()`.
  #   assessment.id: ID of the assessment to be updated (refers to entries in
  #     the `assessment_id` field in the `assessments` table).
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of updated records in the `level_of_evidence` table.

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
  # Marks records as reviewed. Use `GetRecordsToReview()` to obtain a data
  # frame of records to be reviewed.
  #
  # Args:
  #   record.ids: A vector of record IDs to be marked as reviewed. Refers to the
  #     `record_id` field in the `level_of_evidence` table.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of the marked records from the `level_of_evidence` table.

  dbExecute(conn, "UPDATE level_of_evidence
            SET reviewed = 'yes'
            WHERE record_id = ?;", params=list(record.ids))
  reviewed <- GetLoE(query=record.ids, field="record_id", mode="exact", conn=conn)
  return(reviewed)
}

MarkAsNotReviewed <- function(record.ids, conn=eaDB){
  # Marks records as not reviewed. Use `GetRecordsToReview()` to obtain a data
  # frame of records to be reviewed.
  #
  # Args:
  #   record.ids: A vector of record IDs to be marked as not reviewed. Refers to the
  #     `record_id` field in the `level_of_evidence` table.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A data frame of the marked records from the `level_of_evidence` table.
  
  dbExecute(conn, "UPDATE level_of_evidence
            SET reviewed = 'no'
            WHERE record_id = ?;", params=list(record.ids))
  reviewed <- GetLoE(query=record.ids, field="record_id", mode="exact", conn=conn)
  return(reviewed)
}

######################################################################### #
# DELETE FUNCTIONS ########################################################
######################################################################### #

RemoveStudies <- function(study.ids, conn=eaDB){
  # Removes studies from the entire database (including level of evidence,
  # quality scores and checklist answers).
  #
  # Args:
  #   study.ids: IDs of studies to be removed from the database. Refers to
  #     entries in the field `study_id` of the `studies` table.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   The number of studies removed from the database.

  n_rem <- RemoveRecords(ids=study.ids, id.field = "study_id", table="studies")
  return(n_rem)
}

RemoveAssessors <- function(assessor.ids, conn=eaDB){
  # Removes assessors from the entire database (including level of evidence,
  # quality scores and checklist answers).
  #
  # Args:
  #   assessor.ids: IDs of assessors to be removed from the database. Refers to
  #     entries in the field `assessor_id` of the `assessors` table.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   The number of assessors removed from the database.

  n_rem <- RemoveRecords(ids=assessor.ids, id.field = "assessor_id",
                             table="assessors")
  return(n_rem)
}

RemoveAssessments <- function(assessment.ids, conn=eaDB){
  # Removes assessments from the entire database (including level of evidence,
  # quality scores and checklist answers).
  #
  # Args:
  #   assessment.ids: IDs of assessments to be removed from the database.
  #     Refers to entries in the field `assessment_id` of the `assessments`
  #     table.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   The number of assessments removed from the database.

  n_rem <- RemoveRecords(ids=assessment.ids, id.field = "assessment_id", table="assessments")
  return(n_rem)
}

RemoveEvidenceForStudies <-  function(study.ids, conn=eaDB){
  # Removes quality assessment data for studies, but keeps the studies in the
  # database.
  #
  # Args:
  #   study.ids: IDs of studies for which quality assessment data is to be
  #     removed from the database. Refers to entries in the field `study_id` of
  #     the `studies` table.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A list with the numbers of entries removed from the tables
  #   `level_of_evidence` and `quality`.

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
  # Removes quality assessment data for assessments, but keeps the assessments # in the database.
  #
  # Args:
  #   assessment.ids: IDs of assessment for which quality assessment data is to
  #     be removed from the database. Refers to entries in the field
  #     `assessment_id` of the `assessments` table.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A list with the numbers of entries removed from the tables
  #   `level_of_evidence` and `quality`.

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
  # Removes records in the `level_of_evidence` table, but keeps the
  # corresponding studies, assessments and assessors in the database.
  #
  # Args:
  #   record.ids: IDs of records to be removed from the `level_of_evidence`
  #   table. Refers to entries in the field `record_id` of the
  #   `level_of_evidence` table.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A list with the numbers of entries removed from the tables
  #   `level_of_evidence` and `quality`.

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

######################################################################### #
# TEMPLATES ###############################################################
######################################################################### #

TemplateStudies <- function(N=1){
  # Creates a template data frame to be used with `CreateStudies()`
  #
  # Args:
  #   N: Number of rows in the template.
  #
  # Returns:
  #   A data frame with N rows and colummns as needed for use with
  #   `CreateStudies()`
  studies <- data.frame("abbreviation" = character(N),
                        "authors" = character(N),
                        "title" = character(N),
                        "year" = integer(N),
                        "doi" = character(N),
                        stringsAsFactors = FALSE)
  return(studies)
}

TemplateAssessors <- function(N=1){
  # Creates a template data frame to be used with `CreateAssessors()`
  #
  # Args:
  #   N: Number of rows in the template.
  #
  # Returns:
  #   A data frame with N rows and colummns as needed for use with
  #   `CreateAssessors()`
  assessors <- data.frame("name" = character(N),
                          "affiliation" = character(N),
                          "email" = character(N),
                          stringsAsFactors = FALSE)
  return(assessors)
}

TemplateAssessments <- function(N=1){
  # Creates a template data frame to be used with `CreateAssessments()`
  #
  # Args:
  #   N: Number of rows in the template.
  #
  # Returns:
  #   A data frame with N rows and colummns as needed for use with
  #   `CreateAssessments()`
  assessments <- data.frame("assessor_id" = integer(N),
                            "source" = character(N),
                            "date_entered" = character(N),
                            stringsAsFactors = FALSE)
  return(assessments)
}

TemplateAssessStudies <- function(N=1, conn=eaDB){
  # Creates a template data frame to be used with `AssessStudies()`
  #
  # Args:
  #   N: Number of rows in the template.
  #
  # Returns:
  #   A data frame with N rows and colummns as needed for use with
  #   `AssessStudies()`

  # Get number of questions in checklist
  n_questions <- as.integer(dbGetQuery(conn, "SELECT COUNT(question_id)
                                       FROM checklist")[1,1])

  studies_details <- data.frame("study_id" = integer(N),
                                "study_design" = character(N),
                                "res_context" = character(N),
                                "res_focus" = character(N),
                                "res_question" = character(N),
                                "res_outcome" = character(N),
                                stringsAsFactors = FALSE)
  # Prepare columns for answers to checklist questions
  studies_checklist <- matrix(NA, N, n_questions)
  studies_checklist <- as.data.frame(studies_checklist)
  colnames(studies_checklist) <- paste0("q", seq(1:n_questions))

  return(cbind(studies_details, studies_checklist))
}

######################################################################### #
# HELPER FUNCTIONS ########################################################
######################################################################### #

IDsOnly <- function(results, id.field=NULL){
  # Reduces a data frame to one or more ID columns.
  #
  # Args:
  #   results: A data frame to be reduced.
  #   id.field: A character vector containing the names of the columns that
  #     contain IDs. If NULL, all columns that contain `_id` in their names
  #     will be included. Default is NULL.
  #
  # Returns:
  #   The original data frame reduced to its ID columns.

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
  # Replaces duplicate with original IDs in specified tables and deletes
  # duplicates where new ID assignments cause conflicts. THIS FUNCTION IS NOT
  # MEANT TO BE USED ON ITS OWN, AND CAN CREATE INCONSISTENCIES IN THE
  # DATABASE. For removal of duplicates, use the CombineDuplicate functions.
  #
  # Args:
  #   duplicate.ids: A numeric or integer vector containing the IDs of
  #     duplicates (referring to entries in the `id.field` of the specified
  #     table), in the same order as the IDs of the corresponding originals.
  #   original.ids: A numeric or integer vector containing the IDs of the
  #     originals (referring to entries in the `id.field` of the specified
  #     table), in the same order as the IDs of the corresponding duplicates.
  #   id.field: Name of field that contains IDs in the specified table.
  #   sort.field: Name of field that is used to detect conflicts. If #
  #     modifications create records with matching entries in their `id.field`
  #     and `sort.field`, only the records corresponding to the original ID is
  #     kept.
  #   tables: Character vector containing the names of tables within the
  #     database where IDs are to be replaced.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   A list with the numbers of modified and removed records in the specified
  #   tables.

  if(length(duplicate.ids) != length(original.ids)){
    stop("Numbers of duplicate IDs and original IDs don't match.")
  }

  n_ids <- length(duplicate.ids)

  # Prepare lists to store number of changed and deleted records
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

      # Get assessment_ids for duplicates and originals
      sort_ids_dup <- dbGetQuery(conn, q_sort, params=list(duplicate.id))
      sort_ids_org <- dbGetQuery(conn, q_sort, params=list(original.id))
      sort_ids_dup <- unique(sort_ids_dup[,1])
      sort_ids_org <- unique(sort_ids_org[,1])

      # Finding assignments for duplicates that already contain the original
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
  # Removes records from a table in the database. THIS FUNCTION IS NOT MEANT TO
  # BE USED ON ITS OWN, AND CAN CREATE INCONSISTENCIES IN THE DATABASE. For
  # removal of records, use the other, more specific Remove functions.
  #
  # Args:
  #   ids: A vector containing IDs of records to be removed. Referes to entries
  #     in the `id.field` of the specified table.
  #   id.field: The name of the column that
  #     contains IDs in the specified table.
  #   table: The name of the table from which records are to be removed.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #  The number of removed records.

  q_remove <- paste0("DELETE FROM ", dbQuoteIdentifier(conn, table),
                     "WHERE ", dbQuoteIdentifier(conn, id.field), " = ?;")
  n_rem <- dbExecute(conn, q_remove, params=list(ids))
  return(n_rem)
}

UpdateLoE <- function(study.ids=NULL, conn=eaDB){
  # Updates the calculated quality score and level of evidence based on already
  # existing information in the database.
  #
  # Args:
  #   study.ids: Vector containing IDs of studies for which quality score and
  #     level of evidence are to be updated. Refers to the `study_id` field in
  #     the studies table. If NULL, all studies are updated. Default is NULL.
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   Updated records in the level_of_evidence table.

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

######################################################################### #
# TEST DATABASE CONVENIENCE ###############################################
######################################################################### #

ResetTestDB <- function(conn=eaDB){
  # Removes all records from the test database. DO NOT USE WITH THE ACTUAL
  # EVIDENCE ASSESSMENT DATABASE.
  #
  # Args:
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   The combined number of entries removed from all tables.

  dbExecute(conn, "USE evidence_testing;")
  n <- 0
  n <- n + dbExecute(conn, "DELETE FROM studies;")
  n <- n + dbExecute(conn, "DELETE FROM assessors;")
  n <- n + dbExecute(conn, "DELETE FROM assessments;")
  n <- n + dbExecute(conn, "DELETE FROM quality;")
  n <- n + dbExecute(conn, "DELETE FROM level_of_evidence;")
  dbExecute(conn, "ALTER TABLE studies AUTO_INCREMENT = 1")
  dbExecute(conn, "ALTER TABLE assessors AUTO_INCREMENT = 1")
  dbExecute(conn, "ALTER TABLE assessments AUTO_INCREMENT = 1")
  dbExecute(conn, "ALTER TABLE quality AUTO_INCREMENT = 1")
  dbExecute(conn, "ALTER TABLE level_of_evidence AUTO_INCREMENT = 1")
  return(n)
}

ResetTestDBWithDuplicates <- function(conn=eaDB){
  # Removes all records from the test database and enters duplicate
  # information. DO NOT USE WITH THE ACTUAL EVIDENCE ASSESSMENT DATABASE.
  #
  # Args:
  #   conn: A DBIConnection object as returned by dbConnect(); referring to a
  #     MySQL or MariaDB conncection. Default is eaDB.
  #
  # Returns:
  #   NULL

  dbExecute(conn, "USE evidence_testing;")
  ResetTestDB(conn=conn)
  examples <- read.csv("data/example_studies.csv")
  new_studies <- examples[,1:5]
  CreateStudies(studies=new_studies)
  new_assessors <- TemplateAssessors(N=2)
  new_assessors[1,] <- c("Mupepele et al.",
                         "Department of Biometry and Environmental System Analysis, University of Freiburg",
                         "anne-christine.mupepele@biom.uni-freiburg.de")
  new_assessors[2,] <- c("assessor2",
                         "",
                         "ex@mple.com")
  CreateAssessors(assessors=new_assessors)
  new_assessments <- TemplateAssessments(N=1)
  new_assessments[1, ] <- c(1, "https://doi.org/10.1890/15-0595", "2015-11-23")
  CreateAssessments(assessments=new_assessments)
  assess_studies <- TemplateAssessStudies(N=13)
  assess_studies$study_id <- 1:13
  assess_studies[,2:49] <- examples[,c(10, 6:9, 11:53)]
  AssessStudies(studies=assess_studies, assessment.id=1)
  new_assessments <- TemplateAssessments(N=1)
  new_assessments[1, ] <- c(2, "no source", "")
  CreateAssessments(assessments=new_assessments)
  assess_studies <- TemplateAssessStudies(N=13)
  assess_studies$study_id <- 1:13
  assess_studies[,2:49] <- examples[, c(10, 6:9, 11:53)]
  AssessStudies(studies=assess_studies, assessment.id=2)
  new_studies <- GetStudies()
  new_studies <- new_studies[8:9,-1]
  CreateStudies(new_studies, force=TRUE)
  new_assessors <- TemplateAssessors(N=1)
  new_assessors[1,] <- c("Mupepele A.-C..",
                         "University of Freiburg",
                         "no email")
  CreateAssessors(new_assessors, force=TRUE)
  new_assessments <- TemplateAssessments(N=1)
  new_assessments[1, ] <- c(3, "no source", "")
  CreateAssessments(assessments=new_assessments)
  assess_studies <- TemplateAssessStudies(N=14)
  assess_studies$study_id <- 1:14
  assess_studies[,2:49] <- examples[c(1:13,8), c(10, 6:9, 11:53)]
  AssessStudies(studies=assess_studies, assessment.id=3)
  return()
}
