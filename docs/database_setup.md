# Database setup

Setting up a database for evidence assessment requires access to a MySQL or MariaDB server through an account with sufficient rights (i.e. to create databases and users). The necessary SQL scripts are provided in the `src` directory.

## Table of Contents
  * [1. Database creation](#1-database-creation)
  * [2. Database initialization](#2-database-initialization)
  * [3. User management](#3-user-management)
  * [4. Database for testing purposes](#4-database-for-testing-purposes)
  * [5. Removing the database](#5-removing-the-database)

## 1. Database creation
 To create an *empty* database for evidence assessment change the working directory to the project root (`eat_db`) and log into the MySQL or MariaDB server via the command line. Use an account with sufficient rights (usually `root` or another administrator account). For example, log in as `root` from `localhost`:

```bash
cd eat_db
mysql -u root -h localhost -p
```

In MySQL, source the SQL script for setting up the standard [database structure](../fig/eat_db.svg)). The name `evidence_assessment` is used for the new database, and any existing database of this name is *removed* first.

```sql
source src/db_create_mysql.sql;
```

To change the name of the database, modify `db_create_mysql.sql` accordingly (lines 5 and 6). Use the modified name instead of `evidence_assessment` in all subsequent steps.

## 2. Database initialization
Before the database can be used, it needs to be initialized with a quality checklist and downgrading criteria. For the standard [checklist](../checklist), [adjustment ranges](../adjustments.csv) and [downgrading rules](../downgrading.csv), use:

```sql
source src/db_initialize_mysql.sql;
```

Now the tables `study_designs`, `checklist`, `adjustments`, and `downgrading` are initialized. It is not recommend to change these tables after entering assessment information, as this may lead to inconsistencies in the database.

If a different name of the database was used in the first step (i.e. not `evidence_assessment`),`db_initialize_mysql.sql` must be modified accordingly (line 3).

The standard criteria are discussed in more detail in:

> Mupepele, A.-C., Walsh, J. C., Sutherland, W. J., & Dormann, C. F. (2016). An evidence assessment tool for ecosystem services and conservation studies. Ecological Applications, 26(5), 1295–1301. https://doi.org/10.1890/15-0595

## 3. User management
It is recommended to create several new users for tiered access to the database. First (within MySQL), create an admin account with all rights concerning the evidence assessment database. Replace `PASSWORD-ADMIN` with a secure password of your choice.

```sql
CREATE USER 'evidence_admin'@'%' IDENTIFIED BY 'PASSWORD-ADMIN';
GRANT ALL PRIVILEGES ON evidence_assessment.* TO 'evidence_admin'@'%';
```

Second, create a user account that can retrieve and create records, but not update and remove them (and thus neither combine duplicates). Again, replace `PASSWORD-USER` with a secure password of your choice.

```sql
CREATE USER 'evidence_user'@'%' IDENTIFIED BY 'PASSWORD-USER';
GRANT SELECT, INSERT ON evidence_assessment.* TO 'evidence_user'@'%';
```

Finally, create an account without password for read-only purposes (i.e. retrieval of records from the database).

```sql
CREATE USER 'evidence_ro'@'%';
GRANT SELECT ON evidence_assessment.* TO 'evidence_ro'@'%';
```

Either skip creating a read-only user, or provide it with a password, in case information in the database should not be openly accessible.

To create -- or reset -- all accounts at once, use the SQL script `db_users_mysql.sql` within MySQL. Modify the lines that specify passwords accordingly.

```sql
source src/db_users_mysql.sql;
```

## 4. Database for testing purposes

It is recommended to create a database for testing purposes and for getting familiar with the R interface. The SQL script `db_testing_mysql.sql` creates a standard database named `evidence_assessment_test` and the user `evidence_test`, possessing all privileges for the test database, and being identified by the password `dbtest`.

```sql
source src/db_testing_mysql.sql;
```

The script can also be used to recreate an existing test database (returning it to its initial state).

To remove the test database and corresponding user:

```sql
DROP DATABASE IF EXISTS evidence_testing;
DROP USER IF EXISTS 'evidence_test'@'%';
```

## 5. Removing the database
To recreate the evidence assessment database simply follow steps 1 -- 3 again. To completely remove the database and the corresponding accounts execute (in MySQL):

```sql
DROP DATABASE IF EXISTS evidence_assessment;
DROP USER IF EXISTS 'evidence_admin'@'%';
DROP USER IF EXISTS 'evidence_user'@'%';
DROP USER IF EXISTS 'evidence_ro'@'%';
```
