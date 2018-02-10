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

# Create duplicate records for assessment ----
setwd("~/Desktop/evidence assessment")
setwd("./src")
examples <- read.csv("../data/example_studies.csv")
studies <- examples[,1:5]

CreateStudies(studies)
assessors <- data.frame(name=c("Mupepele et al", "assessor2"), email=c("anne-christine.mupepele@biom.uni-freiburg.de","ex@mple.com"))
CreateAssessors(assessors)
assessments <- data.frame(assessor_id=c("1","2"), source=c("Mupepele et al. 2016", "no source"))
CreateAssessments(assessments, date="2015-11-23")

examples <- read.csv("../data/example_studies.csv")
studies <- examples[,1:5]
CreateStudies(studies, force = T)

studies <- TemplateAssessStudies(13)
study_id <- 1:13
studies <- cbind(study_id, examples)
AssessStudies(studies = studies, assessment_id = 1)

studies <- TemplateAssessStudies(13)
study_id <- 14:26
studies <- cbind(study_id, examples)
AssessStudies(studies = studies, assessment_id = 1)
# TODO: FAILS after removing duplicates <-  WHY?

study_id <- 1:13
studies <- cbind(study_id, examples)
AssessStudies(studies = studies, assessment_id = 2)
GetLoE()[1:3]

assessments <- data.frame(assessor_id=2, source="no source")
CreateAssessments(assessments)
studies <- TemplateAssessStudies(13)
study_id <- 1:13
studies <- cbind(study_id, examples)
AssessStudies(studies = studies, assessment_id = 3)

assessments <- data.frame(assessor_id=2, source="no source")
CreateAssessments(assessments, date="2018-01-31")
GetAssessments()
studies <- TemplateAssessStudies(13)
study_id <- 14:26
studies <- cbind(study_id, examples)
AssessStudies(studies = studies, assessment_id = 4)

GetLoE()[,1:3]
dbReadTable(eaDB, "quality")$study_id

#
dbGetQuery(conn, "SELECT assessment_id, study_id FROM quality WHERE study_id = ?;", params=original.id)

# Create template for duplicate matching
examples <- read.csv("../data/example_studies.csv")
studies <- examples[,1:5]
studies <- studies[c(5,2,1,12,9),]

# Update Studies
studies <- examples[2,1:5]
study_id <- 1
UpdateStudies(study_id, studies.update = studies)

AssessStudies(studies = studies, assessment_id = 1)

CombineDuplicateStudies(1, 2)
