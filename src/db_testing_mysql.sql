/* USE ONLY WITH MySQL */

/* Sets up a database and corresponding user account for testing purposes */

DROP DATABASE IF EXISTS evidence_testing;
CREATE DATABASE evidence_testing;

/* User account */
DROP USER IF EXISTS 'evidence_test'@'%';
CREATE USER 'evidence_test'@'%' IDENTIFIED BY 'dbtest'
       WITH MAX_USER_CONNECTIONS 1;
GRANT ALL PRIVILEGES ON evidence_testing.* TO 'evidence_test'@'%';


/* Database structure */
USE evidence_testing;
CREATE TABLE studies(
  study_id      INT           NOT NULL AUTO_INCREMENT,
  abbreviation  VARCHAR(50)   NOT NULL,  -- formatted as AuthorYear
  authors       VARCHAR(500)  NOT NULL,  -- study author(s)
  title         VARCHAR(500)  NOT NULL,  -- study title
  year          INT           NOT NULL,  -- publication year of study
  doi           VARCHAR(50),             -- DOI of study

  /* Keys */
  PRIMARY KEY(study_id)
);


CREATE TABLE assessors(
  assessor_id   INT           NOT NULL AUTO_INCREMENT,
  name          VARCHAR(500)  NOT NULL,  -- name of assessor
  affiliation   VARCHAR(500)  NOT NULL,  -- affiliated institution
  email         VARCHAR(50)   NOT NULL,  -- email contact of assessor

  /* Keys */
  PRIMARY KEY(assessor_id)
);

CREATE TABLE study_designs(
  study_design  VARCHAR(50)   NOT NULL,  -- type of study design
  loe_pre       VARCHAR(5)    NOT NULL,  -- LoE based on study design

  /* Keys */
  PRIMARY KEY(study_design, loe_pre),

  /* Checks */
  CHECK (loe_pre IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4'))
);


CREATE TABLE checklist(
  question_id   INT            NOT NULL,  -- corresponds to number in checklist
  q_group       VARCHAR(50)    NOT NULL,  -- corresponds to group in checklist
  q_subgroup    VARCHAR(50)    NOT NULL,  -- corresponds to subgroup in checklist
  question      VARCHAR(500)   NOT NULL,  -- question text
  description   VARCHAR(1000),              -- question description

  /* Keys */
  PRIMARY KEY(question_id),

  /* Checks */
  CONSTRAINT group_subgroup  -- possible matchings between group and subgroup
       CHECK ((q_group = 'Internal validity' AND
               q_subgroup IN ('Research aim', 'Data collection', 'Analysis', 'Results and Conclusions'))
              OR
              (q_group = 'Design-specific aspects' AND
               q_subgroup IN ('Review', 'Study with a reference/control', 'Observational studies'))
              OR
              (q_group = 'Focus-specific aspects' AND
               q_subgroup IN ('Quantification', 'Valuation', 'Management','Governance')))
);


CREATE TABLE adjustments(
  adjustment_id INT           NOT NULL,  -- adjustment identifier
  q_score_ub    FLOAT         NOT NULL,  -- upper bound (inclusive) of quality score range as percentage
  q_score_lb    FLOAT         NOT NULL,  -- lower bound (exclusive) of quality score range as percentage
  adjustment    VARCHAR(50)   NOT NULL  UNIQUE,  -- adjustment to LoE based on quality score range

  /* Keys */
  PRIMARY KEY (adjustment_id),

  /* Checks */
  CONSTRAINT score_range_ub CHECK (q_score_ub >= -1 AND q_score_ub <=100),
  CONSTRAINT score_range_lb CHECK (q_score_lb >= -10 AND q_score_lb <=100),
  CHECK (q_score_lb <= q_score_ub),
  CHECK (adjustment IN ('none', 'half a level', 'one level', 'one and a half levels',
                         'two levels', 'two and a half levels', 'three levels', 'NA'))
);


CREATE TABLE assessments(
  assessment_id INT   NOT NULL AUTO_INCREMENT,
  assessor_id   INT   NOT NULL,
  date_entered  DATE  NOT NULL,  -- entry date of records in format YYYY-MM-DD
  source        VARCHAR(500),    -- published source of assessments

  /* Keys */
  PRIMARY KEY(assessment_id),
  FOREIGN KEY(assessor_id) REFERENCES assessors(assessor_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);


CREATE TABLE quality(
  assessment_id INT         NOT NULL,
  study_id      INT         NOT NULL,
  question_id   INT         NOT NULL,
  answer        VARCHAR(5)  NOT NULL,

  /* Keys */
  CONSTRAINT quality_pk PRIMARY KEY (assessment_id, study_id, question_id),
  FOREIGN KEY (assessment_id) REFERENCES assessments(assessment_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  FOREIGN KEY (study_id) REFERENCES studies(study_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  FOREIGN KEY (question_id) REFERENCES checklist(question_id),

  /* Checks */
  CHECK (answer IN ('yes', 'no', 'NA'))
);


CREATE TABLE downgrading(
  rule_id       INT             NOT NULL,  -- rule identifier
  adjustment_id INT             NOT NULL,  -- adjustment identifier
  loe_pre       VARCHAR(5)      NOT NULL,  -- LoE based on study designs
  loe_final     VARCHAR(5)      NOT NULL,  -- LoE after downgrading

  /* Keys */
  PRIMARY KEY (rule_id),
  FOREIGN KEY (adjustment_id) REFERENCES adjustments(adjustment_id),

  /* Checks */
  CHECK (loe_pre IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4')),
  CHECK (loe_final IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4'))
);


CREATE TABLE level_of_evidence (
  record_id     INT           NOT NULL AUTO_INCREMENT,
  assessment_id INT           NOT NULL,
  study_id      INT           NOT NULL,
  study_design  VARCHAR(50)   NOT NULL,  -- type of study design
  res_context   VARCHAR(500)  NOT NULL,  -- description of research context
  res_focus     VARCHAR(500)  NOT NULL,  -- type of research focus
  res_question  VARCHAR(500)  NOT NULL,  -- description of research question
  res_outcome   VARCHAR(500)  NOT NULL,  -- description of research outcome
  loe_final     VARCHAR(5)    NOT NULL,  -- LoE after downgrading
  loe_pre       VARCHAR(5)    NOT NULL,  -- LoE based on study design
  points_p      INT           NOT NULL,  -- possible quality points, calculated
  points_q      INT           NOT NULL,  -- quality points achieved, calculated
  q_score       FLOAT,                -- quality score as percentage, calculated
  downgrading   VARCHAR(50)   NOT NULL,  -- downgrading based on quality score
  reviewed      VARCHAR(10)   NOT NULL,  -- whether validity of databse entry has been reviewed

  /* Keys */
  PRIMARY KEY(record_id),
  FOREIGN KEY(assessment_id) REFERENCES assessments(assessment_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  FOREIGN KEY(study_id) REFERENCES studies(study_id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  FOREIGN KEY(downgrading) REFERENCES adjustments(adjustment),
  FOREIGN KEY(study_design, loe_pre) REFERENCES study_designs(study_design, loe_pre),
  /* NB: for new records, the studies, assessor, and assements tables
  must be populated first */

  /* Checks */
  CONSTRAINT assessment_study UNIQUE(assessment_id, study_id),
  CHECK (res_focus IN ('Quantification', 'Valuation', 'Management', 'Governance')),
  CHECK (loe_final IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4')),
  CHECK (loe_pre IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4')),
  CHECK (points_q <= points_p),
  CONSTRAINT score_range CHECK (q_score IS NULL OR (q_score>= -1 AND q_score <=100)),
  CHECK (reviewed IN ('yes', 'no'))
);

/* Initialize study design and corresponding level of evidence*/

INSERT INTO study_designs(study_design, loe_pre)
      VALUES ('Systematic review','LoE1a');

INSERT INTO study_designs(study_design, loe_pre)
      VALUES ('Conventional review','LoE1b');

INSERT INTO study_designs(study_design, loe_pre)
      VALUES ('Case control','LoE2a');

INSERT INTO study_designs(study_design, loe_pre)
      VALUES ('Before-after control-impact','LoE2a');

INSERT INTO study_designs(study_design, loe_pre)
      VALUES ('Multiple lines of moderate evidence','LoE2b');

INSERT INTO study_designs(study_design, loe_pre)
      VALUES ('Observational (Inferential)','LoE3a');

INSERT INTO study_designs(study_design, loe_pre)
      VALUES ('Observational (Descriptive)','LoE3b');

INSERT INTO study_designs(study_design, loe_pre)
      VALUES ('Multiple lines of weak evidence','LoE3c');

INSERT INTO study_designs(study_design, loe_pre)
      VALUES ('Expert opinion','LoE4');

INSERT INTO study_designs(study_design, loe_pre)
      VALUES ('Mechanism-based reasoning','LoE4');


/* Initialize checklist */

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (1,'Internal validity','Research aim','Does the study address a clearly focused question?','See main text, section ''setting question and the context''');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (2,'Internal validity','Research aim','Does the question match the answer?','Answers may not directly correspond to the originally formulated question, e.g. ''Does hunting lead to genetic changes in the moose population of North America?'' is answered by: ''hunting reduces the size of calves''. The missing match is obvious when question and answer are written next to each other, but in publications with much text in between it may be more difficult to identify. The result of reduced calf size may be interesting, but special care should be taken while assessing the evidence base.');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (3,'Internal validity','Data collection','Was the population/area of interest defined in space, time and size?','''Population/area'' is the target, we aim to say something about; e.g. North America''s moose population. ');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (4,'Internal validity','Data collection','Selection bias: Was the sample area representative for the population defined?','Usually samples are not taken from the whole population/area; e.g. only several North American forests were selected to measure moose. Were the selected forests representative? Did they cover the north, south, east and western part of North America? ');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (5,'Internal validity','Data collection','Was the sample size appropriate?','Were the criteria used to determine the sample size (e.g. power calculation) reasonable?');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (6,'Internal validity','Data collection','Was probability/random sampling used for constructing the sample?','Probability sampling means random sampling with known selection probabilities for all objects in the population, while nonprobability sampling does not involve random selection (Trochim, 2014; Söderqvist and Soutukorva, 2009). Most often equal probability sampling is used: e.g. all forests in North America have the same chance of being randomly selected. Unequal probability sampling can be used to ensure representativeness of result, e.g. if a forest in the south of the area is selected, the selection of the next forest far away from the first will be favored. Unequal probability sampling can also mean that forests easy to access obtain a higher selection probability. Probability sampling is important in addition to representative sampling (question 4). ');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (7,'Internal validity','Data collection','If secondary data were used, did an evaluation of the original data take place?','Secondary data, such as used in cost-benefit transfer for example, need to be evaluated to make sure that the data used are not prone to bias.');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (8,'Internal validity','Data collection','If data collection took place in form of a questionnaire, was it pre-tested/piloted?','Questionnaires need to be professionally designed to ensure that they measure what they intend to measure. Therefore a questionnaire should be pre-tested/piloted on a smaller sample size to test its performance (see Rattray & Jones 2007).');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (9,'Internal validity','Data collection','Were the data collection methods described in sufficient detail to permit replication?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (10,'Internal validity','Analysis','Were the statistical/analytical methods described in sufficient detail to permit replication?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (11,'Internal validity','Analysis','Is the choice of statistical/analytical methods appropriate and/or justified?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (12,'Internal validity','Analysis','Was  uncertainty assessed and reported?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (13,'Internal validity','Results and Conclusions','Do the data support the outcome?','Are the conclusions drawn of the analytical results valid?');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (14,'Internal validity','Results and Conclusions','Magnitude of effect: Is the effect large, significant and/or without large uncertainty?','This question aims to identify the magnitude and precision of results. Precise results are usually characterized by low uncertainty (CEBM 2010) and in combination with a large effect the appropriate statistical analysis (question 11) will lead to a significant result. Not all studies allow the judgment of all three aspect and we therefore combine them in one question and recommend context specific decisions.');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (15,'Internal validity','Results and Conclusions','Are all variables and statistical measures  reported?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (16,'Internal validity','Results and Conclusions','Attrition bias: Are non-response/drop-outs given and is their impact discussed?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (17,'Design-specific aspects','Review','Is there a low probability of publication bias?','An assessment of publication bias should include a combination of graphical aids (e.g. funnel plot, other available tests) and/or statistical tests (e.g., Egger regression test, Hedges-Olken) (CEBM 2010). If no quantitative analysis is included, discussion of possible publication bias can be sufficient.');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (18,'Design-specific aspects','Review','Is the review based on several strong-evidence individual studies?','Most ideally every included study should be assessed for its level of evidence. Several strong evidence individual studies should be included to achieve strong evidence in the review. See main text for further details.');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (19,'Design-specific aspects','Review','Do the studies included respond to the same question?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (20,'Design-specific aspects','Review','Are results between individual studies consistent and homogeneous?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (21,'Design-specific aspects','Review','Was the literature searched in a systematic and comprehensive way?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (22,'Design-specific aspects','Review','Was a meta-analysis included?','The term ''meta-analysis'' has been vaguely defined in ecology and conservation (Vetter et al. 2013). In this context we do not talk about any summary analysis (e.g. vote counting), but an explicit meta-analysis as defined by Vetter et al. 2013 or Koricheva et al. 2013');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (23,'Design-specific aspects','Review','Were appropriate a priori study inclusion/exclusion criteria defined?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (24,'Design-specific aspects','Review','Did at least two people select studies and extract data?','At least two people should select papers and extract data. There should be a consensus procedure to resolve any differences (CEBM 2010). In most cases it is too costly to extract data from every paper twice. It might be sufficient to follow the consensus procedure for the first few studies.');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (25,'Design-specific aspects','Study with a reference/control','Allocation bias: Was the assignment of case-control groups randomized?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (26,'Design-specific aspects','Study with a reference/control','Were groups designed equally, aside from the investigated point of interest?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (27,'Design-specific aspects','Study with a reference/control','Performance bias: Was the sampling blinded?','Blinding means that e.g. researchers taking samples of a specific area wouldn''t know the differences between these areas. ');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (28,'Design-specific aspects','Study with a reference/control','Were there sufficient replicates of treatment and reference groups?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (29,'Design-specific aspects','Study with a reference/control','Detection bias: Were outcomes equally measured and determined between groups?','Beside the importance to design groups equally (Question 26), the outcome has to be measured equally. This is necessary to avoid a bias due to the measurement method.');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (30,'Design-specific aspects','Observational studies','Were confounding factors identified and strategies to deal with them stated?','Controlled studies have equally designed groups (Question 26). Observational studies can not be so easily controlled for potential confounders. It is therefore particularly important to identify them and discuss strategies to avoid biasing results.');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (31,'Focus-specific aspects','Quantification','Is the unit of the quantification measurement appropriate?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (32,'Focus-specific aspects','Quantification','Was temporal change (e.g. annual or long-term) of quantities measured (e.g. species abundance or an ecosystem service) discussed?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (33,'Focus-specific aspects','Valuation','If discounting of future costs and outcomes is necessary, was it performed correctly?','Discounting ecosystem services is less straightforward than discounting purely economic values. Nevertheless, it has to be considered when talking about future values (TEEB 2010, ch.6)');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (34,'Focus-specific aspects','Valuation','If aggregate economic values for a population were estimated, was this estimation consistent with the sampling and the definition of the population?','Individual values are summed up to total economic values (TEV), for example in cost-benefit analysis. This should be done thoroughly (e.g. avoiding double counting, considering system boundaries...)');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (35,'Focus-specific aspects','Management','Was the aim of the management intervention clearly defined?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (36,'Focus-specific aspects','Management','Were side effects and trade offs on other non-target species, ecosystem services or stakeholders considered?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (37,'Focus-specific aspects','Management','Were both long-term and short-term effects discussed?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (38,'Focus-specific aspects','Management','Did monitoring take place for an appropriate time period?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (39,'Focus-specific aspects','Management','Appropriate outcome measures: Are all relevant outcomes measured in a reliable way?','Ideally the outcome, e.g. increase in biodiversity, is measured according to an evidence-based quantification or valuation tool.');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (40,'Focus-specific aspects','Governance','Were long-term effects assessed?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (41,'Focus-specific aspects','Governance','Was the policy instrument that was used described?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (42,'Focus-specific aspects','Governance','Was the influence of the applied policy instrument (incentive/law) on the society discussed?','');

INSERT INTO checklist(question_id, q_group, q_subgroup, question, description)
     VALUES (43,'Focus-specific aspects','Governance','Appropriate outcome measures: Are all relevant outcomes measured in a reliable way?','Ideally the outcome, e.g. increase in biodiversity, is measured according to an evidence-based quantification or valuation tool.');

/* Initialize adjustments */

INSERT INTO adjustments(adjustment_id, q_score_ub, q_score_lb, adjustment)
     VALUES (1,100,87,'none');

INSERT INTO adjustments(adjustment_id, q_score_ub, q_score_lb, adjustment)
     VALUES (2,87,74,'half a level');

INSERT INTO adjustments(adjustment_id, q_score_ub, q_score_lb, adjustment)
     VALUES (3,74,61,'one level');

INSERT INTO adjustments(adjustment_id, q_score_ub, q_score_lb, adjustment)
     VALUES (4,61,49,'one and a half levels');

INSERT INTO adjustments(adjustment_id, q_score_ub, q_score_lb, adjustment)
     VALUES (5,49,36,'two levels');

INSERT INTO adjustments(adjustment_id, q_score_ub, q_score_lb, adjustment)
     VALUES (6,36,24,'two and a half levels');

INSERT INTO adjustments(adjustment_id, q_score_ub, q_score_lb, adjustment)
     VALUES (7,24,-0.1,'three levels');

INSERT INTO adjustments(adjustment_id, q_score_ub, q_score_lb, adjustment)
     VALUES (8,-0.1,-10,'NA');


/* Initialize rules for downgrading */

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (1,1,'LoE1a','LoE1a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (2,2,'LoE1a','LoE1b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (3,3,'LoE1a','LoE2a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (4,4,'LoE1a','LoE2b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (5,5,'LoE1a','LoE3a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (6,6,'LoE1a','LoE3b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (7,7,'LoE1a','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (8,1,'LoE1b','LoE1b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (9,2,'LoE1b','LoE2a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (10,3,'LoE1b','LoE2b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (11,4,'LoE1b','LoE3a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (12,5,'LoE1b','LoE3b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (13,6,'LoE1b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (14,7,'LoE1b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (15,1,'LoE2a','LoE2a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (16,2,'LoE2a','LoE2b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (17,3,'LoE2a','LoE3a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (18,4,'LoE2a','LoE3b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (19,5,'LoE2a','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (20,6,'LoE2a','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (21,7,'LoE2a','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (22,1,'LoE2b','LoE2b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (23,2,'LoE2b','LoE3a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (24,3,'LoE2b','LoE3b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (25,4,'LoE2b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (26,5,'LoE2b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (27,6,'LoE2b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (28,7,'LoE2b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (29,1,'LoE3a','LoE3a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (30,2,'LoE3a','LoE3b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (31,3,'LoE3a','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (32,4,'LoE3a','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (33,5,'LoE3a','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (34,6,'LoE3a','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (35,7,'LoE3a','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (36,1,'LoE3b','LoE3b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (37,2,'LoE3b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (38,3,'LoE3b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (39,4,'LoE3b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (40,5,'LoE3b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (41,6,'LoE3b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (42,7,'LoE3b','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (43,1,'LoE3c','LoE3c');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (44,2,'LoE3c','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (45,3,'LoE3c','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (46,4,'LoE3c','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (47,5,'LoE3c','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (48,6,'LoE3c','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (49,7,'LoE3c','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (50,1,'LoE4','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (51,2,'LoE4','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (52,3,'LoE4','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (53,4,'LoE4','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (54,5,'LoE4','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (55,6,'LoE4','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (56,7,'LoE4','LoE4');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (57,8,'LoE1a','LoE1a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (58,8,'LoE1b','LoE1b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (59,8,'LoE2a','LoE2a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (60,8,'LoE2b','LoE2b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (61,8,'LoE3a','LoE3a');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (62,8,'LoE3b','LoE3b');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (63,8,'LoE3c','LoE3c');

INSERT INTO downgrading(rule_id, adjustment_id, loe_pre, loe_final)
     VALUES (64,8,'LoE4','LoE4');
