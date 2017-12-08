Tools to create and manage a relational database based on the evidence assessment tool by Mupepele et al. (2016).

## Create Database
To create an *empty* database in SQLite for evidence assessment, navigate to the project root and run:

    sqlite3 db/loe.db < src/create_db_sqlite.sql

To initialize the empty database with the standard checklist and downgrading rules as provided by Mupepele et al. (2016):

    sqlite3 db/loe.db < src/initialize_db_sqlite.sql


## Database Structure

![erd](/fig/eat_db.svg)
