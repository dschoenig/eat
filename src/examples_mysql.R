setwd("~/Desktop/evidence assessment")
setwd("./src")

# install.packages("DBI", "RMariaDB")

# source("functions_mysql.R")

# Establish database connections
con <- dbConnect(RMariaDB::MariaDB(), host="localhost", user="evidence_admin", password="biometry101", dbname="evidence_assessment")
dbDisconnect(con)
# Check initiliazed assessment tables
dbGetQuery(con, 'SELECT * FROM study_designs')
dbGetQuery(con, 'SELECT * FROM checklist')
dbGetQuery(con, 'SELECT * FROM adjustments')
dbGetQuery(con, 'SELECT * FROM downgrading')


# Check tables for data entry (should be empty)
dbGetQuery(con, 'SELECT * FROM assessors')
dbGetQuery(con, 'SELECT * FROM studies')
dbGetQuery(con, 'SELECT * FROM assessments')
dbGetQuery(con, 'SELECT * FROM quality')
dbGetQuery(con, 'SELECT * FROM level_of_evidence')



# General workflow: 1. Create Assesor; 2. Create Study; 3. Create Assessment; 4. Enter assessment data

# 1. Register new assessors

assessors <- data.frame(name=c("Mupepele et al.", "assessor2"), email=c("anne-christine.mupepele@biom.uni-freiburg.de","ex@mple.com"))
 
new_assessors(assessors=assessors)

# Query assessors
get_assesor_ids()
get_assesor_ids(select="mu", field="email")
get_assesor_ids(select="freiburg", field="email")

# TODO: Ensure functionality with MySQL
# 2. Register new studies

examples <- read.csv("../data/example_studies.csv")
new_studies(examples[,1:5])

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

study_id <- get_study_ids(examples[,1], mode = "abbreviation")[,1]
studies <- cbind(study_id, examples[,6:10])
answers <- examples[,11:53]

assess_study(studies = studies, assessment_id = 1, answers = answers)

# Query database
full_record <- dbGetQuery(con, "SELECT *
                                FROM studies
                                JOIN level_of_evidence JOIN assessments JOIN assessors
                                ON studies.study_id = level_of_evidence.study_id
                                AND assessments.assessment_id = level_of_evidence.assessment_id
                                AND assessments.assessor_id = assessors.assessor_id;")
names(full_record)
full_record[1,]



# Disconnect from database
dbDisconnect(con)
