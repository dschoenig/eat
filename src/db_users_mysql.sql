DROP USER IF EXISTS 'evidence_admin'@'localhost';

CREATE USER 'evidence_admin'@'localhost' IDENTIFIED BY 'biometry101';
GRANT ALL PRIVILEGES ON evidence_assessment.* TO 'evidence_admin'@'localhost';

FLUSH PRIVILEGES;
