# 0. Getting started

setwd("eat_db")
source("src/interface_mysql.R")

# 1. Connecting to the database

# Admin account
eaDB <- dbConnect(RMariaDB::MariaDB(),
                  host="127.0.0.1",
                  user="evidence_admin",
                  password="PASSWORD-ADMIN",
                  dbname="evidence_assessment")
# User account
eaDB <- dbConnect(RMariaDB::MariaDB(),
                  host="127.0.0.1",
                  user="evidence_user",
                  password="PASSWORD-USER",
                  dbname="evidence_assessment")

# Read-only account
eaDB <- dbConnect(RMariaDB::MariaDB(),
                  host="127.0.0.1",
                  user="evidence_ro",
                  dbname="evidence_assessment")

dbDisconnect(eaDB)

eaDB <- dbConnect(RMariaDB::MariaDB(),
                  host="127.0.0.1",
                  user="evidence_test",
                  password="dbtest",
                  dbname="evidence_testing")
dbExecute(eaDB, "SET SESSION wait_timeout=300;")

ResetTestDB()

# 2. Data entry

# Read in example data
examples <- read.csv("data/example_studies.csv")

# We will register 13 studies. Let's have a look at how the corresponding data
# frame should be laid out.
TemplateStudies(N=13)

# This corresponds to the first five columns of our examples data frame
names(examples)
new_studies <- examples[,1:5]

# Create entries for studies in the database
CreateStudies(studies=new_studies)

# We will register 2 new assessors
new_assessors <- TemplateAssessors(N=2)

# Let's have a look at all registered assessors, to make sure that we will use the correct assessor_id
GetAssessors()

# Information about first assessor to be registered
new_assessors[1,] <- c("Mupepele et al.", 
                       "Department of Biometry and Environmental System Analysis, University of Freiburg", 
                       "anne-christine.mupepele@biom.uni-freiburg.de")
# Example for second assessor
new_assessors[2,] <- c("assessor2", 
                       "", 
                       "ex@mple.com")

# Register new assessors
CreateAssessors(assessors=new_assessors)

# We will register 1 assessment
new_assessments <- TemplateAssessments(N=1)

# Let's have a look at all registered assessors, to make sure that we will use 
# the correct assessor_id
GetAssessors()

# The assessment has been conducted by the assessor with ID 1 (Mupepele et al.).
# The source of the assessment is the corresponding paper
new_assessments[1, ] <- c(1, "https://doi.org/10.1890/15-0595")

# Register new assessment, using the acceptance date of the publication
CreateAssessments(assessments=new_assessments, date="2015-11-23")

# Let's take another look at the studies in the database and extract the studies
# table of the database into a data frame
studies <- GetStudies() 
studies

# Let's review all registered assessments as well
GetAssessments()
# The assessment_id we will use is 1

# We will assess 13 studies
assess_studies <- TemplateAssessStudies(N=13)

# Enter the study_ids into the template.
# All studies will form part of the assessment, so we will use all study_ids
assess_studies$study_id <- studies$study_id

# All other relevant information can be found in columns 6 to 53 of the examples
names(examples)
names(assess_studies)
# Note that the ordering of the columns in examples is not the same as in our
# template, so we will make an adjustment when copying.
names(examples[,c(10, 6:9, 11:53)])
names(assess_studies[,2:49])

# Entering information from the examples, with adjusting column ordering.
assess_studies[,2:49] <- examples[,c(10, 6:9, 11:53)]

# Enter evidence assessment information to calculate quality scores and
# determine the level of evidence for each study.
AssessStudies(studies=assess_studies, assessment.id=1)


# 3. Data retrieval

# 4. Handling duplicate entries
# 5. Updating records
# 6. Removing records


setwd("~/Desktop/evidence assessment")

# install.packages("DBI", "RMariaDB")

source("src/functions_mysql.R")

# Establish database connections
eaDB <- dbConnect(RMariaDB::MariaDB(), host="localhost", user="evidence_admin", password="biometry101", dbname="evidence_assessment")

eaDB <- dbConnect(RMariaDB::MariaDB(), host="localhost", user="evidence_user", dbname="evidence_assessment")

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

CreateStudies(studies, force=T)
GetStudies("eco", "title", mode = "fuzzy", fuzzy.min.sim = 0.1, ids.only = T)
GetRecords(select = "millar2010", field="abbreviation", table="studies", return.fields = c("abbreviation"), ids.only = F, mode="fuzzy", fuzzy.min.sim = 0.1)

# 2. Register new assessor
assessors <- data.frame(name=c("Mupepele et al", "assessor2"), email=c("anne-christine.mupepele@biom.uni-freiburg.de","ex@mple.com"))

CreateAssessors(assessors, force = T)
CheckForDuplicateAssessors(assessors)
CheckForDuplicateAssessors()
GetAssessors()
CheckForDuplicateStudies(studies)

# 3. Register new assessment

# We will register 1 assessment
new_assessments <- TemplateAssessments(N=1)

# We will use the assessor with ID 1 (i.e. Muepele et al.). 
# The source of the assessment will be the corresponding paper
new_assessments[1, ] <- c(1, "https://doi.org/10.1890/15-0595")

# Register new assessment, dated to acceptance date of the publication
CreateAssessments(assessments=new_assessments, date="2015-11-23")


# 4. Assess studies
# change to fill template step by step
studies <- TemplateAssessStudies(13)
study_id <- GetStudies(examples[,1], field="abbreviation", mode="exact", ids.only = T)
studies <- cbind(study_id, examples)
studies$study_id
AssessStudies(studies = studies, assessment.id = 1)

GetFullRecords(select="Observtional", field = "loe.study_design", mode="fuzzy", fuzzy.min.sim = 0.1)

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

CreateStudies(studies, force = T)
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

study_id <- 14:26
studies <- cbind(study_id, examples)

colnames(examples)
assess_study(studies = studies, assessment_id = 1, answers = answers)

# Query database



# Disconnect from database
dbDisconnect(con)

# Additional comments: database name difference
