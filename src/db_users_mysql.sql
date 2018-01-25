DROP USER IF EXISTS 'evidence_admin'@'localhost';

CREATE USER 'evidence_admin'@'localhost' IDENTIFIED BY 'biometry101';
GRANT ALL PRIVILEGES ON evidence_assessment.* TO 'evidence_admin'@'localhost';

CREATE USER 'evidence_user'@'localhost';
GRANT SELECT, INSERT ON evidence_assessment.* TO 'evidence_user'@'localhost';

FLUSH PRIVILEGES;
