# R interface for the evidence assessment database
The [R interface](../src/interface_mysql.R) for the evidence assessment database is designed to allow retrieval, creation, updating, removal and duplicate handling of records in the database. It depends on a correctly structured and initialized MySQL or MariaDB database. Please refer to the [SQL documentation]([testing database](database_setup.md) for the evidence assessment database to ensure that your database has been set up correctly.

The interface functions have been built on top of the DBI and RMariaDB packages. These packages offer general purpose functions to interact with the evidence assessment database in ways that are not covered by the R interface. In addition, the interface requires the packages stringsdist and tidyr.

Some functions will not work for database users with restricted rights. In particular, all `CombineDuplicates` functions, all `Remove` functions, and `ReassessStudies` require the user to possess `DELETE` rights.

The database structure allows the same studies to be assessed multiple times, even by the same assessor.

## 0. Getting started
All functions pertaining to the interface are contained in the script `src/interface_mysql.R`. Refer to this script for more detailed documentation of each function and its arguments.

This tutorial assumes that a [test database](database_setup.md#4-database-for-testing-purposes) has been properly set up. Example assessments taken from Mupepele et al. (2016) are provided in `data/example_studies.csv.`

To load the interface, change your working directory to the project root and source `src/interface_mysql.R`. The script will check for dependencies and install the required packages if necessary.

```r
setwd("eat_db")
source("src/interface_mysql.R")
```

## 1. Connecting to the database

The functions `dbConnect()` (package DBI), and `MariaDB()` (package RMariaDB) are used to connect to the evidence assessment database. Connection requires to specify

1. The address of the MySQL or MariaDB server
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

Similarly, a connection as a standard user, or read-only user can be established. For example:

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

For the remaining part of this tutorial, the test database will be used. Make sure to use the correct host IP. For example:

```r
eaDB <- dbConnect(RMariaDB::MariaDB(),
                  host="127.0.0.1",
                  user="evidence_test",
                  password="dbtest",
                  dbname="evidence_testing")
dbExecute(eaDB, "SET SESSION wait_timeout=300;")
```

The testing database accepts only one connection at a time for the user `evidence_test`. Therefore we set a timeout of 5 minutes of inactivity, after which the connection will be automatically closed. Also close the connection with `dbDisconnect(eaDB)` as soon as you are finished.

It is recommend to reset the test before continuing with the tutorial. Use `ResetTestDB()` for this purpose.

```r
ResetTestDB()
```

## 2. Data entry
Data entry will be demonstrated by entering the assessments from Mupepele et al. (2016) into the database.

The general workflow consists in
1. Entering general information for the studies to be assessed (in case the studies are not already in the database).
2. Register a new assessor (or choose one that is already registered).
3. Register a new assessment (or choose an already existing assessment to be amended).
4. Enter quality assessment information for studies to be assessed.

### 2.1 Register new studies
After reading in the example data, we will use `CreateStudies()` to register new studies to be assessed. `CreateStudies()` requires a data frame containing one row for each study, and columns containing general information, such as an `abbreviation` (preferably in the format firstauthorYYYY, with YYYY being year of publication), `authors`, `title`, `year` of publication, and `doi` (see [database structure](../fig/eat_db.svg)). `TemplateStudies()` can be used to create an empty data frame with the correct layout.

```r
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
```
`CreateStudies()` will return all entries that have been added to the database.

Note that a unique `study_id` has been assigned to each study. These IDs will be used when entering evidence assessment information later ([section 2.4](#2-4-enter-evidence-assessment-information)). Studies in the database can be retrieved with `GetStudies()` ([section 3](#3-data-retrival)).

The function performs a check for duplicates, which can (but generally should not) be disabled by using the argument `force=TRUE` (see the section on [duplicate handling](#4-handling-duplicate-entries) for more details).

### 2.2 Register new assessors
We will use `CreateAssessors()` to register a new assessors. Similar to `CreateStudies()`, `CreateAssessors()` requires a data frame containing one row for each assessor, and columns to identify the assessor. Information provided should include name, affiliation, and email (see [database structure](../fig/eat_db.svg)). Again, `TemplateAssessors()` can be used to create an empty data frame with the correct layout.

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

Again, `CreateAssessors()` will all entries that have been added to the database.

Each assessor has been assigned a unique `assessor_id`, which will be used to register a new assessment in [section 2.3](2-3-create-new-assessment). Assessors in the database can be retrieved with `GetAssessors()` ([section 3](#3-data-retrival)).

`CreateAssessors()` also performs [duplicate checking](#4-handling-duplicate-entries), which can be disabled by `force=TRUE`.

### 2.3 Register new assessment
The last step before entering assessment data consists in registering a new assessment with `CreateAssessment()`. `CreateAssessment()` requires a data frame as produced with `TemplateAssessment()`. It must contain columns `assessor_id` and `source`. Entries in `assessor_id` must refer to a valid `assessor_id` in the database (see [section 2.2](#2-2-registering-new-assessors)); while entries in `source` are optional and identify the source where the assessment has been published.

The `date=` argument of `CreateAssessment()` can be used to assign a date to the assessments, in the format YYYY-MM-DD. If no date is provided, the function will default to the current system date.

```r
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
```

`CreateAssessment()` will return the newly registered assessments. Each assessment has been assigned an `assessment_id`, which will be required in ([section 2.4](#2-4-enter-evidence-assessment-information)).

`CreateAssessment()` does not perform any duplicate checking.

### 2.4 Enter evidence assessment information
Entering assessment information requires
1. The `study_id` of the studies in the database to be assessed [section 2.1](2-1-registering-new-studies).
2. The `assessment_id` of the corresponding assessment [section 2.3](2-3-create-new-assessment).
3. A data frame that holds all information related assessing the level of evidence for each study, including answers to questions of the quality checklist. For more information refer to Mupepele et al. (2016). Quality score and level of evidence are determined within the database.

Required are *study design* (`study_design`), *research context* (`res_context`), *research focus* (`res_focus`), *research question* (`res_question`), and *research outcome* (`res_outcome`). For these fields, the following restrictions apply:

> **study_design**: for the standard implementation, must be one of `Systematic review`, `Conventional review`, `Before-after control-impact`, `Case control`, `Multiple lines of moderate evidence`, `Observational (Inferential)`, `Observational (Descriptive)`, `Multiple lines of weak evidence`, `Mechanism-based reasoning`, `Expert opinion`.
> **res_focus**: for the standard implementation, must be one of `Quantification`, `Valuation`, `Management`, `Governance`.

The data frame must also contain one column for each question of the quality checklist, named `q1`, `q2`, … , `q43`. Answers to questions must be entered as `1` (yes), or `0` (no); everything else will be considered as `NA` (question does not apply).

```r
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
```
`AssessStudies()` returns a summary of the evidence assessment, including level of evidence based on study design (`loe_pre`), quality points possible (`points_p`), quality points achieved (`points_q`), quality score in percent (`q_score`), the resulting `downgrading`,  and the final level of evidence (`loe_final`).

`GetLoE()` and `GetFullRecords` can be used to retrieve more exhaustive records (see [section 3](#3-data-retrival)).

## 3. Data retrieval
## 4. Handling duplicate entries
## 5. Updating records
## 6. Removing records
## References
