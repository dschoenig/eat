Tools to create, use and manage a relational database that implements the evidence assessment tool by Mupepele et al. (2016).

>Mupepele, A.-C., Walsh, J. C., Sutherland, W. J., & Dormann, C. F. (2016). An evidence assessment tool for ecosystem services and conservation studies. Ecological Applications, 26(5), 1295–1301. <https://doi.org/10.1890/15-0595>

## Overview
[Database structure](#database-structure)  
[Database setup](#database-setup)  
[R interface](#r-interface)  

## Database Structure

![erd](fig/eat_db.svg)

## Database setup
**[Setup Guide](docs/database_setup.md)**

  * [1. Database creation](docs/database_setup.md#1-database-creation)
  * [2. Database initialization](docs/database_setup.md#2-database-initialization)
  * [3. User management](docs/database_setup.md#3-user-management)
  * [4. Database for testing purposes](docs/database_setup.md#4-database-for-testing-purposes)
  * [5. Removing the database](docs/database_setup.md#5-removing-the-database)


## R interface
**[Interface documentation](docs/r_interface.md)**

  * [Overview](docs/r_interface.md#overview)
  * [0. Getting started](docs/r_interface.md#0-getting-started)
  * [1. Connecting to the database](docs/r_interface.md#1-connecting-to-the-database)
  * [2. Data entry](docs/r_interface.md#2-data-entry)
     * [2.0 Quick examples](docs/r_interface.md#20-quick-examples)
     * [2.1 Register new studies](docs/r_interface.md#21-register-new-studies)
     * [2.2 Register new assessors](docs/r_interface.md#22-register-new-assessors)
     * [2.3 Register new assessments](docs/r_interface.md#23-register-new-assessments)
     * [2.4 Enter evidence assessment information](docs/r_interface.md#24-enter-evidence-assessment-information)
     * [2.5 Working with already registered information](docs/r_interface.md#25-working-with-already-registered-information)
  * [3. Data retrieval](docs/r_interface.md#3-data-retrieval)
  * [4. Handling duplicate entries](docs/r_interface.md#4-handling-duplicate-entries)
     * [4.1 Checking for duplicates](docs/r_interface.md#41-checking-for-duplicates)
     * [4.2 Combining duplicates](docs/r_interface.md#42-combining-duplicates)
  * [5. Updating records](docs/r_interface.md#5-updating-records)
  * [6. Reviewing records](docs/r_interface.md#6-reviewing-records)
  * [7. Removing records](docs/r_interface.md#7-removing-records)
  * [References](docs/r_interface.md#references)
