# Database setup

Setting up a database for evidence assessment requires access to a MySQL server through an account with rights to create new databases and new users. Corresponding SQL scripts are provided in the src directory.

## 1. Database creation
 To create an *empty* database for evidence assessment log into the MySQL or MariaDB server via the command line. Use an account with sufficient rights (usually `root` or another administrator account). To make sourcing the scripts easier, first change the working directory to the `evidence_assessment` folder. For example (login from `localhost`):

```sh
cd evidence_assessment
mysql -u root -h localhost -p
```

In MySQL source the SQL script for setting up the database structure (see the [standard structure](../fig/eat_db.svg). Make sure to point to the correct location of the script. The name `evidence_assessment` is used for the new database and any existing database of this name is *removed* first.

```sql
source src/db_create_mysql.sql;
```

To change the name of the database, modify db_create_mysql.sql accordingly (lines 5 and 6). Then use this name instead of `evidence_assessment` in all subsequent steps.

## 2. Database initialization
Before the database can be used, it needs to be initialized with a quality checklist and downgrading criteria. For the standard checklist and criteria, use

```sql
source src/db_create_mysql.sql;
```

Now the tables `study_designs`, `checklist`, `adjustments`, and `downgrading` are initialized. It is not recommend to change these tables after entering assessment information, as this may lead to inconsistencies in the database.

If a different name of the database was used in the first step (i.e. not `evidence_assessment`), the script `db_create_mysql.sql` must be modified accordingly (line 3).

The standard criteria follow Mupepele et al. (2016).

> Mupepele, A.-C., Walsh, J. C., Sutherland, W. J., & Dormann, C. F. (2016). An evidence assessment tool for ecosystem services and conservation studies. Ecological Applications, 26(5), 1295–1301. https://doi.org/10.1890/15-0595

## 3. User management
It is recommended to create several new users for tiered access to the database. First (within MySQL), create an admin account with with all rights concerning the evidence assessment database. Replace `PASSWORD-ADMIN` with a secure password of your choice.

```sql
CREATE USER 'evidence_admin'@'%' IDENTIFIED BY 'PASSWORD-ADMIN';
GRANT ALL PRIVILEGES ON evidence_assessment.* TO 'evidence_admin'@'%';
```

Second, create a user account that can retrieve and create records, but not update and remove (and thus neither combine duplicates). Again, replace `PASSWORD-USER` with a secure password of your choice.

```sql
CREATE USER 'evidence_user'@'%' IDENTIFIED BY 'PASSWORD-USER';
GRANT SELECT, INSERT ON evidence_assessment.* TO 'evidence_user'@'%';
```

Finally, create an account without passqord for read-only purposes (i.e. retrieval of records from the database).

```sql
CREATE USER 'evidence_ro'@'%';
GRANT SELECT ON evidence_assessment.* TO 'evidence_ro'@'%';
```

If the database should not be accessible without a password, either skip provide a password, or skip creating a read-only user.

To create -- or reset -- all accounts at once, use the SQL script `db_users_mysql.sql` within MySQL. Modify the lines providing passwords accordingly.

```sql
source src/db_users_mysql.sql;
```

## 4. Database for testing purposes

It is recommended to create a database for testing purposes and for getting familiar with the R interface. The SQL script `db_testing_mysql.sql` creates a standard database named `evidence_assessment_test` and a user account `evidence_test` with complete privileges for the database, and identified by the password `dbtest`. Within MySQL, use

```sql
source src/db_testing_mysql.sql;
```

The script can also be used to recreate the testing database (i.e. return it to its initial state).

To remove the database and corresponding user:

```sql
DROP DATABASE IF EXISTS evidence_testing;
DROP USER IF EXISTS 'evidence_test'@'%';
FLUSH PRIVILEGES;
```

## 5. Removing the database
To recreate the database simply follow steps 1 -- 3 again. To completely remove the database and the corresponding accounts within MySQL:

```sql
DROP DATABASE IF EXISTS evidence_assessment;
DROP USER IF EXISTS 'evidence_admin'@'%';
DROP USER IF EXISTS 'evidence_user'@'%';
DROP USER IF EXISTS 'evidence_ro'@'%';
FLUSH PRIVILEGES;
```
