setwd("~/Desktop/evidence assessment")
setwd("./src")

# install.packages("DBI", "RMariaDB")

# source("functions_mysql.R")

# Establish database connections
eaDB <- dbConnect(RMariaDB::MariaDB(), host="localhost", user="evidence_admin", password="biometry101", dbname="evidence_assessment")
dbDisconnect(eaDB)

# Check initiliazed assessment tables
dbGetQuery(eaDB, 'SELECT * FROM study_designs')
dbGetQuery(eaDB, 'SELECT * FROM checklist')
dbGetQuery(eaDB, 'SELECT * FROM adjustments')
dbGetQuery(eaDB, 'SELECT * FROM downgrading')


# Check tables for data entry (should be empty)
dbGetQuery(eaDB, 'SELECT * FROM assessors')
dbGetQuery(eaDB, 'SELECT * FROM studies')
dbGetQuery(eaDB, 'SELECT * FROM assessments')
dbGetQuery(eaDB, 'SELECT * FROM quality')
dbGetQuery(eaDB, 'SELECT * FROM level_of_evidence')



# General workflow: 1. Create Assesor; 2. Create Study; 3. Create Assessment; 4. Enter assessment data

# 1 Register new studies
examples <- read.csv("../data/example_studies.csv")
studies <- examples[,1:5]

CreateStudies(studies)
GetStudies("eco", "title")

# 2. Register new assessor
assessors <- data.frame(name=c("Mupepele et al.", "assessor2"), email=c("anne-christine.mupepele@biom.uni-freiburg.de","ex@mple.com"))

CreateAssessors(assessors)
GetAssessors()

# 3. Register new assessment

assessments <- data.frame(assessor_id=c("1","2"), source=c("Mupepele et al. 2016", "no source"))
CreateAssessments(assessments, date="2015-11-23")
GetAssessments()


# 4. Assess studies
# change to fill template step by step
studies <- TemplateAssessStudies(13)
study_id <- GetStudies(examples[,1], field="abbreviation", mode="exact")
studies$study_id <- study_id$study_id
studies <- cbind(study_id$study_id, examples)

colnames(examples)
AssessStudies(studies = studies, assessment_id = 1)
assess_study(studies = studies, assessment_id = 1, answers = answers)
GetFullRecords()
GetFullRecords(ids.only = T)
GetStudies(select=2006:2015, field="year", ids.only = F)
GetStudies(select=2010:2018, field="year", ids.only = T)
GetStudies(select=2018:2010, field="year", ids.only = T)

# Old Examples ----

# 1. Register new assessors

assessors <- data.frame(name=c("Mupepele et al.", "assessor2"), email=c("anne-christine.mupepele@biom.uni-freiburg.de","ex@mple.com"))
 
new_assessors(assessors=assessors)

# Query assessors
get_assesor_ids()
get_assesor_ids(select="mu", field="email")
get_assesor_ids(select="freiburg", field="email")

# 2. Register new studies

examples <- read.csv("../data/example_studies.csv")
studies <- examples[,1:5]

CreateStudies(studies)
GetStudies("eco", "title")

# Query studies
get_study_ids()
get_study_ids(examples[c(2,1),1], mode = "abbreviation")
get_study_ids(examples[,5], mode = "doi")
get_study_ids(c("ac", "liu"), mode = "author")
get_study_info()
get_study_info(ids=c(1,5))

# 3. Start new assessment
new_assessment(assessor_id = "1", source = "Mupepele et al. 2016", date="2015-11-23")
new_assessment(assessor_id = "3", source = "")

get_assessment_ids()
get_assessment_ids(query="1", mode="assessor_id")
get_assessment_ids(query="2017-12-08", mode="date")
get_assessment_ids(query="2017-12-08", mode="before")
get_assessment_ids(query="2017-12-08", mode="after")

# 4. Assess studies
# change to fill template step by step
studies <- TemplateAssessStudies(13)
study_id <- GetStudies(examples[,1], field="abbreviation", mode="exact")
studies$study_id <- study_id$study_id
studies <- cbind(study_id$study_id, examples)

colnames(examples)
assess_study(studies = studies, assessment_id = 1, answers = answers)

# Query database



# Disconnect from database
dbDisconnect(con)

# Additional comments: database name difference
