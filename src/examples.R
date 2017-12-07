library(DBI)
source("functions.R")

# Establish database connections
eat <- dbConnect(RSQLite::SQLite(), "../db/loe.db")

# Check initiliazed assessment tables
dbGetQuery(eat, 'SELECT * FROM study_designs')
dbGetQuery(eat, 'SELECT * FROM checklist')
dbGetQuery(eat, 'SELECT * FROM adjustments')
dbGetQuery(eat, 'SELECT * FROM downgrading')

# Check tables for data entry
dbGetQuery(eat, 'SELECT * FROM assessors')

# General workflow: 1. Create Assesor; 2. Create Study; 3. Create Assessment; 4. Enter assessment data

# Register new assessors
assessors <- data.frame(name=c("Mupepele et al.", "assessor3"), email=c("anne-christine.mupepele@biom.uni-freiburg.de","ex@mple.com"))

new_assessors(assessors=assessors)

# Disconnect from database
dbDisconnect(eat)




