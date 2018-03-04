# The following examples are extracted from the R interface guide (docs/r_interface.md).

######################################################################### #
# 0. Getting started ######################################################
######################################################################### #

setwd("eat_db")
source("src/interface_mysql.R")

######################################################################### #
# 1. Connecting to the database ###########################################
######################################################################### #

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

# Connect to test database
eaDB <- dbConnect(RMariaDB::MariaDB(),
                  host="127.0.0.1",
                  user="evidence_test",
                  password="dbtest",
                  dbname="evidence_testing")
dbExecute(eaDB, "SET SESSION wait_timeout=300;")

ResetTestDB()

######################################################################### #
# 2. Data entry ###########################################################
######################################################################### #

# 2.0 ---------------------------------------------------------------------
# Connect to the test database (if you have not opened a connection already).
# For example:
eaDB <- dbConnect(RMariaDB::MariaDB(),
                  host="127.0.0.1",
                  user="evidence_test",
                  password="dbtest",
                  dbname="evidence_testing")
dbExecute(eaDB, "SET SESSION wait_timeout=300;")

# Reset the database
ResetTestDB()

# Example 1 ---------------------------

# Step 1
# Import example data
examples <- read.csv("data/example_studies.csv")
# Prepare data frame for registering new studies
new_studies <- examples[,1:5]
# Register new studies
CreateStudies(studies=new_studies)

# Step 2
# Prepare data frame for registering new assessors
new_assessors <- TemplateAssessors(N=2)
new_assessors[1,] <- c("Mupepele et al.",
                       "Department of Biometry and Environmental System Analysis, University of Freiburg",
                       "anne-christine.mupepele@biom.uni-freiburg.de")
new_assessors[2,] <- c("assessor2",
                       "",
                       "ex@mple.com")
# Register new assessors
CreateAssessors(assessors=new_assessors)

# Step 3
# Prepare data frame for registering new assessments
new_assessments <- TemplateAssessments(N=1)
new_assessments[1, ] <- c(1, "https://doi.org/10.1890/15-0595", "2015-11-23")
# Register new assessment
CreateAssessments(assessments=new_assessments)

# Step 4
# Prepare data frame for entering evidence assessment information
assess_studies <- TemplateAssessStudies(N=13)
assess_studies$study_id <- 1:13 # Studies with study_ids 1 to 13
assess_studies[,2:49] <- examples[,c(10, 6:9, 11:53)]
# Perform assessment
AssessStudies(studies=assess_studies, assessment.id=1)

# Example 2 ---------------------------

# If studies and assessors are already registered (e.g. after running example
# 1), the process begins with step 3. Here, a new assessment is created for the
# second assessor that will involve only five of the studies in the database.

# Step 3
# Prepare data frame for registering new assessments
new_assessments <- TemplateAssessments(N=1)
new_assessments[1, ] <- c(2, "no source", "")
# Register new assessment, using today's date
CreateAssessments(assessments=new_assessments)

# Step 4
# Prepare data frame for entering evidence assessment information
assess_studies <- TemplateAssessStudies(N=5)
assess_studies$study_id <- 1:5 # Only studies with study_id 1 to 5
assess_studies[,2:49] <- examples[1:5, c(10, 6:9, 11:53)]
# Perform assessment
AssessStudies(studies=assess_studies, assessment.id=2)

# Example 3 ---------------------------

# An existing assessments can be amended simply by performing step 4. Here,
# studies with study_id 6 to 13 will be amended to the assessment conducted in
# example 2.

# Step 4
# Prepare data frame for entering evidence assessment information
assess_studies <- TemplateAssessStudies(N=8)
assess_studies$study_id <- 6:13 # Only studies with study_id 6 to 13
assess_studies[,2:49] <- examples[6:13, c(10, 6:9, 11:53)]
# Perform assessment
AssessStudies(studies=assess_studies, assessment.id=2)

# 2.1 ---------------------------------------------------------------------

# First, reset the test database
ResetTestDB()

# Import example data
examples <- read.csv("data/example_studies.csv")

# We will register 13 studies. Let's have a look at how the corresponding data
# frame should be laid out.
TemplateStudies(N=13)

# This corresponds to the first five columns of our examples data frame
names(examples)
new_studies <- examples[,1:5]

# Create entries for studies in the database
CreateStudies(studies=new_studies)

# 2.2 ---------------------------------------------------------------------

# We will register 2 new assessors
new_assessors <- TemplateAssessors(N=2)

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

# 2.3 ---------------------------------------------------------------------

# We will register 1 assessment
new_assessments <- TemplateAssessments(N=1)

# Let's have a look at all registered assessors, to make sure that we will use
# the correct assessor_id
GetAssessors()

# The assessment has been conducted by the assessor with ID 1; the source of the
# assessment is the corresponding paper
new_assessments[1, ] <- c(1, "https://doi.org/10.1890/15-0595", "2015-11-23")

# Register new assessment
CreateAssessments(assessments=new_assessments)

# 2.4 ---------------------------------------------------------------------

# Let's take another look at the studies in the database and extract the studies
# table of the database into a data frame
studies <- GetStudies()
studies
# We will enter information for all studies and therefore use study_id 1 to 13

# Let's review all registered assessments as well
GetAssessments()
# The assessment_id we will use is 1

# Template for assessing 13 studies
assess_studies <- TemplateAssessStudies(N=13)

# Enter the study_ids 1 to 13into the template.
assess_studies$study_id <- 1:13

# All other relevant information can be found in columns 6 to 53 of the examples
names(examples)
names(assess_studies)
# Note that the ordering of the columns in examples is not the same as in our
# template, so we will make an adjustment when copying.
names(examples[,c(10, 6:9, 11:53)])
names(assess_studies[,2:49])

# Entering information from the examples, with adjusted column order
assess_studies[,2:49] <- examples[,c(10, 6:9, 11:53)]

# Enter evidence assessment information to calculate quality scores and
# determine the level of evidence for each study.
AssessStudies(studies=assess_studies, assessment.id=1)

# Running the last command again will raise an error because each study may only
# be assessed once per assessment.
AssessStudies(studies=assess_studies, assessment.id=1)

# 2.5 ---------------------------------------------------------------------

# We will enter a new assessment conducted by assessor 2 and involving studies
# with the study_id 1 to 5.

# Review the IDs of the studies and assessors registered in the database.
GetStudies()
GetAssessors()

# Prepare data frame for registering a new assessment
new_assessments <- TemplateAssessments(N=1)
# Enter assessor_id and specify published source into template
new_assessments[1, ] <- c(2, "no source", "")
# Register new assessment
CreateAssessments(assessments=new_assessments)

# Prepare data frame for entering evidence assessment information
assess_studies <- TemplateAssessStudies(N=5)
assess_studies$study_id <- 1:5 # Only studies with study_id 1 to 5
assess_studies[,2:49] <- examples[1:5, c(10, 6:9, 11:53)]
# Perform assessment
AssessStudies(studies=assess_studies, assessment.id=2)

# Studies with study_id 6 to 13 will be amended to assessment 2

# Step 4
# Prepare data frame for entering evidence assessment information
assess_studies <- TemplateAssessStudies(N=8)
assess_studies$study_id <- 6:13 # Only studies with study_id 6 to 13
assess_studies[,2:49] <- examples[6:13, c(10, 6:9, 11:53)]
# Perform assessment
AssessStudies(studies=assess_studies, assessment.id=2)

######################################################################### #
# 3. Data retrieval #######################################################
######################################################################### #

# By default Get…() functions will return all records
GetLoE()
GetStudies()
GetAssessors()
GetAssessments()

# GetFullRecords() is used to extract all available information into one data frame
export <- GetFullRecords()
export

# You can choose the columns to include. This will not work with
# GetFullRecords()
GetLoE(return.fields = c("study_id", "assessment_id", "study_design",
                         "loe_pre", "q_score", "loe_final"))
GetStudies(return.fields = c("study_id", "abbreviation", "year", "doi"))

# It is possible to return only columns that contain IDs
GetLoE(ids.only=TRUE)
GetStudies(ids.only=TRUE)

# To return a subset of records, fields of the table can be queried

# Query for highest level of evidence only
GetLoE(query="LoE1a",
       field="loe_final",
       return.fields = c("study_id", "assessment_id", "study_design",
                         "loe_pre", "q_score", "loe_final"))
GetFullRecords(query="LoE1a",
               field="loe_final")

# Find records with LoE1a, LoE1b, LoE2a, or LoE2b
GetLoE(query=c("LoE1a", "LoE1b", "LoE2a", "LoE2b"),
       field="loe_final",
       return.fields = c("study_id", "assessment_id", "study_design",
                         "loe_pre", "q_score", "loe_final"))

# Find studies published between 2010 and 2015
GetStudies(query=2010:2015,
           field="year",
           return.fields = c("study_id", "abbreviation", "year", "doi"))

# In addition to exact matching, partial and fuzzy matching are possible.
# If we want to find studies that contain "eco" in their titles, exact matching
# will not work
GetStudies(query="eco",
           field="title")

# Setting mode to partial returns all results containing "eco"
GetStudies(query="eco",
           field="title",
           mode="partial")
GetStudies(query="eco",
           field="title",
           mode="partial",
           ids.only = TRUE)

# You can also use fuzzy matching
GetStudies(query="tree mortality determinants for dry environments",
           field="title",
           mode="fuzzy",
           return.fields = c("study_id", "title"))

# Sensitivity of fuzzy matching can be controlled with fuzzy.min.sim
GetStudies(query="tree mortality determinants for dry environments",
           field="title",
           mode="fuzzy", fuzzy.min.sim = 0.8)
GetStudies(query="tree mortality determinants for dry environments",
           field="title",
           mode="fuzzy", fuzzy.min.sim = 0.6,
           return.fields = c("study_id", "title"))

######################################################################### #
# 4. Handling duplicate entries ###########################################
######################################################################### #

# 4.1 ---------------------------------------------------------------------

# First, extract all records from the studies table
new_studies <- GetStudies()
# Select studies with study_id 5 and 6; and remove study_id column
new_studies <- new_studies[8:9,-1]
# Trying to add these studies a second time will raise an error
CreateStudies(studies=new_studies)

# Let's have a more detailed look on the duplicates in our source data frame
CheckForDuplicateStudies(studies=new_studies)
# By default, only the first matching field is included in the results. Also,
# similar matches will not be shown if there is already an exact match. It is
# possible to show all matches by setting all.entries=TRUE
CheckForDuplicateStudies(studies=new_studies, all.entries=TRUE)

# You can force the addition of duplicates
CreateStudies(studies=new_studies, force=TRUE)

# An internal duplicate checking can be performed by calling
# CheckForDuplicateStudies() without any argument
CheckForDuplicateStudies()
# Again, it is possible to show only ID columns
CheckForDuplicateStudies(ids.only=TRUE)

# Duplicate checking also works for assessors
new_assessors <- TemplateAssessors(N=1)
new_assessors[1,] <- c("Mupepele A.-C..",
                       "University of Freiburg",
                       "no email")
CreateAssessors(assessors=new_assessors)
CheckForDuplicateAssessors(assessors=new_assessors)

# 4.2 ---------------------------------------------------------------------

# Calling ResetTestDBWithDuplicates() will reset the test database and create
# duplicate records.
ResetTestDBWithDuplicates()

# Looking at the studies table again we see that there are two duplicates
CheckForDuplicateStudies(ids.only = TRUE)
# Study 14 is a duplicate of 8; and study 15 a duplicate of 9
# These studies also form part of several assessments
GetLoE(query=c(8, 9, 14, 15), field="study_id", ids.only=TRUE)
# There are also duplicate assessors and assessments in the database
CheckForDuplicateAssessors()
# Assessor 3 seems to be a duplicate of assessor 1
GetAssessors()
GetAssessments()

# We will now combine duplicate records

# Combine study 14 with study 8, and study 15 with study 9
CombineDuplicateStudies(duplicate.ids=c(14,15), original.ids=c(8,9))
# Check whether this has been successful
CheckForDuplicateStudies(ids.only = TRUE)
GetLoE(query=c(8, 9, 14, 15), field="study_id", ids.only=TRUE)

# Combine assessor 3 with assessor 1
CombineDuplicateAssessors(duplicate.ids=3, original.ids=1)
# Check whether this has been successful
CheckForDuplicateAssessors()
GetAssessors()
GetAssessments() # assessment 3 is now linked to assessor 1
GetLoE(ids.only=TRUE) # no change in level_of_evidence table

# Finally, we will combine assessments 3 and 1
CombineDuplicateAssessments(duplicate.ids=3, original.ids=1)
# Check whether this has been successful
GetAssessments()
GetLoE(ids.only=TRUE) # assessment 3 has been combined with 1

######################################################################### #
# 5. Updating records #####################################################
######################################################################### #

# Update studies 3 to 7
update_ids <- 3:7
# Prepare data frame for updating studies
update_studies <- TemplateStudies(N=5)
# Fill template
update_studies[,1:5] <- examples[3:7,1:5]
# Update study information
UpdateStudies(study.ids=update_ids, studies.update=update_studies)

# Update information for assessor 1
update_ids <- 1
# Prepare data frame for updating assessors
update_assessors <- TemplateAssessors(N=1)
# Fill template
update_assessors[1,] <- c("Mupepele et al.",
                          "University of Freiburg",
                          "anne-christine.mupepele@biom.uni-freiburg.de")
# Update assessor information
UpdateAssessors(assessor.ids=update_ids, assessors.update=update_assessors)

# Update information for assessment 1
update_ids <- 1
# Prepare data frame for updating assessments
update_assessments <- TemplateAssessments(N=1)
# Fill template
update_assessments[1,] <- c("1", "Mupepele et al. (2016)", "")
# Update assessment information
UpdateAssessments(assessment.ids=update_ids, assessments.update=update_assessments)

# ReassessStudies() is used for updating evidence assessment information and
# works identical to AssessStudies()
# Prepare data frame for entering updated evidence assessment information
reassess_studies <- TemplateAssessStudies(N=13)
reassess_studies$study_id <- 1:13 # Studies with study_ids 1 to 13
reassess_studies[,2:49] <- examples[,c(10, 6:9, 11:53)]
# Update assessment with ID 1
ReassessStudies(studies=reassess_studies, assessment.id=1)

######################################################################### #
# 6. Reviewing records ####################################################
######################################################################### #

# Retrieve records that have not been reviewed yet
review <- GetRecordsToReview()
review
GetRecordsToReview(ids.only=TRUE)

# Mark records as reviewed
MarkAsReviewed(record.ids=14:18)

# Mark records as not reviewed
MarkAsNotReviewed(record.ids=14:18)

######################################################################### #
# 7. Removing records #####################################################
######################################################################### #

# Remove studies with study_id 4 and 11 from the database
RemoveStudies(study.ids=c(4, 11))
GetStudies(ids.only=TRUE) # studies have been removed
GetLoE(ids.only=TRUE) # records for studies 4 and 11 have been removed

# Remove evidence assessment information (i.e. single records) from
# level_of_evidence table  but keep the corresponding studies and assessments
# registered in the database
RemoveEvidence(record.ids=43) # remove record with record_id 43
GetLoE(ids.only=TRUE) # record 43 has been removed
GetAssessments() # assessment 1 is still registered
GetStudies(ids.only=TRUE) # study 3 is still registered

# Remove evidence assessment information for study with study_id 5, but keep the
# study registered in the database
RemoveEvidenceForStudies(study.ids=5)
GetLoE(ids.only=TRUE) # all records for study 5 have been removed
GetStudies(ids.only=TRUE) # study 5 is still registered

# Remove evidence assessment information for assessment 2, but keep the
# assessment registered in the database
RemoveEvidenceForAssessments(assessment.ids=2)
GetLoE(ids.only=TRUE) # all records for assessment 2 have been removed
GetAssessments() # assessment 2 is still registered

# Remove assessor with assessor_id 2
RemoveAssessors(assessor.ids = 2)
GetAssessors() # assessor 2 has been removed
GetLoE(ids.only=TRUE) # all records involving assessor 2 have been removed
GetAssessments() # assessments involving assessor 2 have been removed

# Remove assessment with assessment_id 1
RemoveAssessments(assessment.ids=1)
GetAssessments() # assessment 1 has been removed
GetLoE(ids.only=TRUE) # all records involving assessment 1 have been removed
GetAssessors() # assessor 1 is still registered

# Reset the test database
ResetTestDB()
