# R functions to interact with the evidence assessment database

new_assessors <- function(assessors, silent=FALSE){
  # assessors: either a named list (for one assessor) or a data.frame (for several assessors) with columns "name" and "email"
  
  assessors <- data.frame(lapply(assessors, as.character), stringsAsFactors = FALSE) # Coerce input
  
  # Check whether assessors are already in database and remove duplicates from entry dataframe
  duplicates <- dbGetQuery(eat, "SELECT * FROM assessors WHERE name = :name", param=list(name=assessors$name))
  if(nrow(duplicates) != 0){
    duplicate_assessors <- unique(duplicates$name)
    assessors_new <- subset(assessors, !(assessors$name %in% duplicate_assessors))
    warning("One or more assessors are already registered.")
  } else {
    assessors_new <-  assessors 
  }
  
  n_entries <- nrow(assessors_new) # number of new entries
  
  if(n_entries > 0){
    for(i in n_entries){
      # SQL statement for insertion
      insert_assessor <- dbSendStatement(eat,
                                         "INSERT INTO assessors(name, email)
                                         VALUES(:name, :email);")
      dbBind(insert_assessor, param=list(name=assessors_new$name,email=assessors_new$email))  
      dbClearResult(insert_assessor)
    }
  }
  
  # Get assigned IDs
  if(silent == FALSE){
    assessor_ids <- dbGetQuery(eat, "SELECT * FROM assessors WHERE name = :name", param=list(name=assessors$name))
    return(assessor_ids)
  }
}

new_studies <- function(studies){}

new_assessment <- function(studies, study_id, assessor_id, source, date){}

study_information <- function(info, study_id){}

study_checklist <- function(study_id, assessor_id){}

assess_loe <- function(){}
  
# TODO
# Create study, assessor, assessment, respectively
# Get IDs for study, assessor, assessment, respectively
# Create different template dataframe to enter data
# Calculate LoE based on answers to questions
