DROP USER IF EXISTS 'evidence_admin'@'%';
CREATE USER 'evidence_admin'@'%' IDENTIFIED BY 'PASSWORD-ADMIN';
GRANT ALL PRIVILEGES ON evidence_assessment.* TO 'evidence_admin'@'%';

DROP USER IF EXISTS 'evidence_user'@'%';
CREATE USER 'evidence_user'@'%' IDENTIFIED BY 'PASSWORD-USER';
GRANT SELECT, INSERT ON evidence_assessment.* TO 'evidence_user'@'%';

DROP USER IF EXISTS 'evidence_ro'@'%';
CREATE USER 'evidence_ro'@'%';
GRANT SELECT ON evidence_assessment.* TO 'evidence_ro'@'%';
