# Using the R interface

*Less talk, more action? See the quick examples on [data entry](#20-quick-examples), [data retrieval](#3-data-retrieval), duplicate [finding ](#41-checking-for-duplicates) and [combining](#42-combining-duplicates), as well as [updating](#5-updating-records), [reviewing](#6-reviewing-records) and [removing](#7-removing-records) records.*

## Table of Contents
  * [Overview](#overview)
  * [0. Getting started](#0-getting-started)
  * [1. Connecting to the database](#1-connecting-to-the-database)
  * [2. Data entry](#2-data-entry)
     * [2.0 Quick examples](#20-quick-examples)
     * [2.1 Register new studies](#21-register-new-studies)
     * [2.2 Register new assessors](#22-register-new-assessors)
     * [2.3 Register new assessments](#23-register-new-assessments)
     * [2.4 Enter evidence assessment information](#24-enter-evidence-assessment-information)
     * [2.5 Working with already registered information](#25-working-with-already-registered-information)
  * [3. Data retrieval](#3-data-retrieval)
  * [4. Handling duplicate entries](#4-handling-duplicate-entries)
     * [4.1 Checking for duplicates](#41-checking-for-duplicates)
     * [4.2 Combining duplicates](#42-combining-duplicates)
  * [5. Updating records](#5-updating-records)
  * [6. Reviewing records](#6-reviewing-records)
  * [7. Removing records](#7-removing-records)
  * [References](#references)

## Overview
The [R interface](../src/interface_mysql.R) for the evidence assessment database is designed to allow retrieval, creation, updating, removal and duplicate handling of records in the database. It depends on a correctly structured and initialized MySQL or MariaDB database. Please refer to the [setup guide](database_setup.md) to set up the evidence assessment database correctly.

The interface functions have been built on top of the `DBI` and `RMariaDB` packages. The DBI package offers general purpose functions to interact with the evidence assessment database in ways that are not covered by this R interface. In addition, the interface requires the packages `stringsdist` and `tidyr`.

Some functions will not work for database users with restricted rights. In particular, all `CombineDuplicates` functions, all `Remove` functions, and `ReassessStudies` require the user to possess `DELETE` rights.

The [database structure](../fig/eat_db.svg) allows the same studies to be assessed multiple times, even by the same assessor.

## 0. Getting started
All functions pertaining to the interface are contained in the [interface source](../src/interface_mysql.R). Use the source for more detailed documentation of each function and its arguments.

This tutorial assumes that a [test database](database_setup.md#4-database-for-testing-purposes) has been properly set up. Example assessments taken from Mupepele et al. (2016) are provided in `data/example_studies.csv.`

To load the interface, change your working directory to the project root and source `src/interface_mysql.R`. The script will check for dependencies and install the required packages if necessary.

```r
setwd("eat_db")
source("src/interface_mysql.R")
```

## 1. Connecting to the database

The functions `dbConnect()` (package DBI), and `MariaDB()` (package RMariaDB) are used to connect to the evidence assessment database. This requires:

1. The host address of the MySQL or MariaDB server
2. A user account on the server (and the corresponding password)
3. The name of the database (default is `evidence_assessment`)

User accounts and the database name are created during [database setup](database_setup.md).

For example, to connect to a database named `evidence_assessment` hosted on a server with the IP `127.0.0.1` as the user `evidence_admin` with the password `PASSWORD-ADMIN`:

```r
# Admin account
eaDB <- dbConnect(RMariaDB::MariaDB(),
                  host="127.0.0.1",
                  user="evidence_admin",
                  password="PASSWORD-ADMIN",
                  dbname="evidence_assessment")
```

It is important to properly assign the resulting connection object, in this case to `eaDB`. All functions of the R interface default to a connection named `eaDB`. If you prefer to use a different name, you have to specify the connection in the `conn=` argument of each function (refer to the function documentation in `interface_mysql.R` for more details).

Similarly, a connection as a standard user, or as a read-only user can be established, if these accounts were created during databas setup.

```r
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
```

You can disconnect from the database using `dbDisconnect()`:

```r
dbDisconnect(eaDB)
```

For the remaining part of this tutorial, the test database will be used. Make sure to use the correct host address. For example:

```r
eaDB <- dbConnect(RMariaDB::MariaDB(),
                  host="127.0.0.1",
                  user="evidence_test",
                  password="dbtest",
                  dbname="evidence_testing")
dbExecute(eaDB, "SET SESSION wait_timeout=300;")
```

The testing database accepts only one connection at a time for the user `evidence_test`. Therefore we set a timeout of 5 minutes of inactivity, after which the connection will be automatically closed. Also close the connection with `dbDisconnect(eaDB)` as soon as you are finished.

It is recommended to reset the test database before continuing with the tutorial. Use `ResetTestDB()` for this purpose.

```r
ResetTestDB()
```

## 2. Data entry

Data entry will be demonstrated by entering the assessments from Mupepele et al. (2016) into the database.

The general workflow comprises

1. Entering general information for the studies to be assessed (in case the studies are not already registered in the database).
2. Register a new assessor (or choose one that is already registered).
3. Register a new assessment (or choose an already existing assessment to be amended).
4. Enter quality assessment information for studies to be assessed.

### 2.0 Quick examples

These are minimal examples including all necessary steps. If you choose to run them before continuing with the more detailed descriptions in the following sections, you can wipe the database with `ResetTestDB()` after finishing.

```r
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

# Example 1 ---------------------------------------------------------------

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

# Example 2 ---------------------------------------------------------------

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


# Example 3 ---------------------------------------------------------------

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
```

### 2.1 Register new studies
After importing the example data, we will use `CreateStudies()` to register new studies to be assessed. `CreateStudies()` requires a data frame containing one row for each study, and five columns containing general information, such as `abbreviation` (preferably in the format firstauthorYYYY, with YYYY being year of publication), `authors`, `title`, `year` of publication, and `doi` (see [database structure](../fig/eat_db.svg)). `TemplateStudies()` can be used to create an empty data frame with the correct layout.

```r
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
```
`CreateStudies()` will return all entries that have been added to the database.

Note that a unique `study_id` has been assigned to each study. These IDs will be used when entering evidence assessment information later ([section 2.4](#24-enter-evidence-assessment-information)). Studies in the database can be retrieved with `GetStudies()` ([section 3](#3-data-retrieval)).

`CreateStudies()` performs a check for duplicates, which can be disabled by using the argument `force=TRUE` in case you are completely sure that you are not adding duplicates (see the section on [duplicate handling](#4-handling-duplicate-entries) for more details).

### 2.2 Register new assessors
We will use `CreateAssessors()` to register new assessors. Similar to `CreateStudies()`, `CreateAssessors()` requires a data frame containing one row for each assessor, and several columns to identify the assessor. Information provided should include `name`, `affiliation`, and `email` (see [database structure](../fig/eat_db.svg)). Again, `TemplateAssessors()` can be used to create an empty data frame with the correct layout.

```r
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
```

`CreateAssessors()` will return all entries that have been added to the database.

Each assessor has been assigned a unique `assessor_id`, which will be used to register a new assessment in [section 2.3](#23-register-new-assessments). Assessors in the database can be retrieved with `GetAssessors()` ([section 3](#3-data-retrival)).

`CreateAssessors()` also performs [duplicate checking](#41-checking-for-duplicates), which can be disabled by `force=TRUE`.

### 2.3 Register new assessments
The last step before entering assessment data consists in registering a new assessment with `CreateAssessment()`. `CreateAssessment()` requires a data frame as produced with `TemplateAssessment()`. It must contain columns `assessor_id`, `source`, and `date_entered`. Entries in `assessor_id` must refer to a valid `assessor_id` in the database (see [section 2.2](#22-register-new-assessors)); while entries in `source` are optional and identify the source where the assessment has been published. The field `date_entered` should contain dates in the format YYYY-MM-DD. If no date is provided, the current system date will be used.

```r
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
```

`CreateAssessment()` will return the newly registered assessments. Each assessment has been assigned an `assessment_id`, which will be required in ([section 2.4](#24-enter-evidence-assessment-information)).

`CreateAssessment()` does not perform any duplicate checking.

### 2.4 Enter evidence assessment information
Entering assessment information requires

1. The `study_id` of the studies in the database to be assessed ([section 2.1](#21-register-new-studies)).
2. The `assessment_id` of the corresponding assessment ([section 2.3](#23-register-new-assessments)).
3. A data frame that holds all information related to assessing the level of evidence for each study, including answers to questions of the quality checklist. For more information, refer to Mupepele et al. (2016). Quality score and level of evidence are determined within the database.

Again, `TemplateAssessStudies()` produces a template data frame for assessment.

Information provided must include *study design* (`study_design`), *research context* (`res_context`), *research focus* (`res_focus`), *research question* (`res_question`), and *research outcome* (`res_outcome`). For these fields, the following restrictions apply:

> **study_design**: For the standard implementation, must be one of `Systematic review`, `Conventional review`, `Before-after control-impact`, `Case control`, `Multiple lines of moderate evidence`, `Observational (Inferential)`, `Observational (Descriptive)`, `Multiple lines of weak evidence`, `Mechanism-based reasoning`, `Expert opinion`.

> **res_focus**: For the standard implementation, must be one of `Quantification`, `Valuation`, `Management`, `Governance`.

The data frame must also contain one column for each question of the quality checklist, named `q1`, `q2`, … , `q43`. Answers to questions must be entered as `1` ("yes"), or `0` ("no"); everything else will be interpreted as `NA` ("question does not apply").

```r
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
```

`AssessStudies()` returns a summary of the evidence assessment, including level of evidence based on study design (`loe_pre`), quality points possible (`points_p`), quality points achieved (`points_q`), quality score in percent (`q_score`), the resulting `downgrading`,  and the final level of evidence (`loe_final`).

`GetLoE()` and `GetFullRecords` can be used to retrieve more exhaustive records (see [section 3](#3-data-retrieval)).

`AssessStudies()` will not allow to assess a study twice for the same assessment. To assess a study multiple times, new assessments have to be registered. Evidence assessment information can be updated with `ReassessStudies()` ([section 5])

### 2.5 Working with already registered information

Studies and assessors only have to be registered once. The corresponding IDs (`study_id` and `assessor_id`) can be used to conduct additional assessments by repeating steps 3 and 4.

```r
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

# Step 4
# Prepare data frame for entering evidence assessment information
assess_studies <- TemplateAssessStudies(N=5)
assess_studies$study_id <- 1:5 # Only studies with study_id 1 to 5
assess_studies[,2:49] <- examples[1:5, c(10, 6:9, 11:53)]
# Perform assessment
AssessStudies(studies=assess_studies, assessment.id=2)
```

Similarly, an already existing assessment can be amended by repeating step 4.

```r
# Studies with study_id 6 to 13 will be amended to assessment 2

# Step 4
# Prepare data frame for entering evidence assessment information
assess_studies <- TemplateAssessStudies(N=8)
assess_studies$study_id <- 6:13 # Only studies with study_id 6 to 13
assess_studies[,2:49] <- examples[6:13, c(10, 6:9, 11:53)]
# Perform assessment
AssessStudies(studies=assess_studies, assessment.id=2)
```

## 3. Data retrieval

The functions `GetLoE()`, `GetStudies()`, `GetAssessors()`, `GetAssessments()`,  can be used to query records in the `level_of_evidence`, `studies`, `assessors`, and `assessments` tables, respectively. `GetFullRecords()` collapses all information into one data frame that can be exported.

Without any argument, `Get…()` functions will return all records. The following arguments can be used to influence output:

- **`query`**: Query term for the field to be queried. A character vector of several query terms can be used for modes `"exact"` and `"partial"`. If `NULL`, the entire table will be returned. Default is `NULL`.
- **`field`**: Field (i.e. column) to be queried in table. If `NULL`, the entire table will be returned. Default is `NULL`.
- **`return.fields`**: Fields to be included in the returned data frame. If `NULL`, all fields will be returned. Default is `NULL`.
- **`ids.only`**: If `TRUE`, only fields that contain IDs are returned. If `FALSE`, fields to be returned will depend on `return.fields`. Default is `FALSE`.
- **`mode`**: Offers three modes for matching the query term; `"exact"` will only return exact matches to the query term; `"partial"` will also include partial matches; `"fuzzy"` matches similar terms based on a minimum similarity set by `fuzzy.min.sim`. Default is `"exact"`.
- **`fuzzy.min.sim`**: Minimum similarity for `mode="fuzzy"`. Ranges from `0` (everything matches) to `1` (only exact matches). Default is `0.7`.

```r
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
```

## 4. Handling duplicate entries

### 4.1 Checking for duplicates

Duplicate studies and assessors in the database can be detected with the `CheckForDuplicateStudies()` and `CheckForDuplicateAssessors()` functions. If called without any arguments, these functions will look for duplicates already present within the database. By providing a data frame that could be used for the corresponding `Create…()` functions, the data frame will be matched against entries in the database.

Duplicate checking involves exact and fuzzy matching. However, fuzzy matches are not returned if an exact match has already been found for the same combination of entries. Similarly, only the first matching field will be returned. To show all matches, set `all.entries=TRUE`. As with the `Get…()` functions ([section 3](#3-data-retrieval)), the argument `ids.only = TRUE` can be used to return ID columns only.

```r
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
```

The fields used for duplicate checking, and the similarity thresholds for fuzzy matching can also be controlled. For more information refer to the documentation in the [R interface source](../src/interface_mysql.R).

### 4.2 Combining duplicates

Duplicate studies, assessors, and assessments can be combined with `CombineDuplicateStudies()`, `CombineDuplicateAssessors()`, and `CombineDuplicateAssessments()`, respectively. All functions take as arguments a vector of duplicate IDs and a vector of the corresponding original ID for each duplicate (provided in the same order). Any conflicts (e.g. in the `level_of_evidence` table) are solved in favor of the original.

```r
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

```

## 5. Updating records

Several `Update…()` functions can be used to update records in the database. Usage is generally similar to the corresponding `Create…()` functions. Records are updated completely, i.e. a data frame containing all necessary fields must be provided. The `Template…()` functions can again be used to produce such data frames.

`Update…()` functions also require a vector containing the IDs of the records to be updated. IDs must be provided in the same order as the corresponding entries in the data frame that is used for updating.

`ReassessStudies()` is used to update evidence assessment information. As with `AssessStudies()`, quality score and level of evidence will be determined during the process.

```r
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
```

## 6. Reviewing records
After data entry, records should be reviewed by a database administrator to ensure correctness. `GetRecordsToReview()`, `MarkAsReviewed()`, and `MarkAsNotReview()` serve as convenience functions for this purpose.

```r
# Retrieve records that have not been reviewed yet
review <- GetRecordsToReview()
review
GetRecordsToReview(ids.only=TRUE)

# Mark records as reviewed
MarkAsReviewed(record.ids=14:18)

# Mark records as not reviewed
MarkAsNotReviewed(record.ids=14:18)
```

## 7. Removing records

Studies, assessors, assessments, and quality information can be removed from the database. The corresponding `Remove…()` functions are designed to maintain database integrity, and make use of cascading deletes where possible. Specifically, these functions prevent hanging entries in `quality` table.

```r
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
```

## References
Mupepele, A.-C., Walsh, J. C., Sutherland, W. J., & Dormann, C. F. (2016). An evidence assessment tool for ecosystem services and conservation studies. Ecological Applications, 26(5), 1295–1301. <https://doi.org/10.1890/15-0595>

R Core Team. (2017). R: A Language and Environment for Statistical Computing. Vienna, Austria. Retrieved from <https://www.R-project.org/>

R Special Interest Group on Databases (R-SIG-DB), Wickham, H., & Müller, K. (2017). DBI: R Database Interface. Retrieved from <https://CRAN.R-project.org/package=DBI>

Müller, K., Ooms, J., James, D., DebRoy, S., Wickham, H., & Horner, J. (2017). RMariaDB: Database Interface and “MariaDB” Driver. Retrieved from <https://CRAN.R-project.org/package=RMariaDB>

van der Loo, M. P. J. (2014). The stringdist package for approximate string matching. The R Journal, 6, 111–122.

Wickham, H., & Henry, L. (2018). tidyr: Easily Tidy Data with “spread()” and “gather()” Functions. Retrieved from <https://CRAN.R-project.org/package=tidyr>
