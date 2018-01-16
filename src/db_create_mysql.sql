/* USE ONLY WITH MySQL */

DROP DATABASE IF EXISTS evidence_assessment;

CREATE DATABASE evidence_assessment;
USE evidence_assessment;

CREATE TABLE studies(
  study_id      INT           NOT NULL,  AUTO_INCREMENT,
  abbreviation  VARCHAR(50)   NOT NULL,  -- formatted as AuthorYear
  authors       VARCHAR(500)  NOT NULL,  -- study author(s)
  title         VARCHAR(500)  NOT NULL,  -- study title
  year          INT           NOT NULL,  -- publication year of study
  doi           VARCHAR(50),             -- DOI of study

  /* Keys */
  PRIMARY KEY(study_id)
);


CREATE TABLE assessors(
  assessor_id   INT           NOT NULL,  AUTO_INCREMENT,
  name          VARCHAR(500)  NOT NULL,  -- name of assessor
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
  q_score_ub    INT           NOT NULL,  -- upper bound (inclusive) of quality score range as percentage
  q_score_lb    INT           NOT NULL,  -- lower bound (exclusive) of quality score range as percentage
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
  assessment_id INT   NOT NULL,  AUTO_INCREMENT,
  assessor_id   INT   NOT NULL,
  date_entered  INT   NOT NULL,  -- entry date of records in format YYYY-MM-DD
  source        VARCHAR(500),    -- published source of assessments

  /* Keys */
  PRIMARY KEY(assessment_id),
  FOREIGN KEY(assessor_id) REFERENCES assessors(assessor_id)
);


CREATE TABLE quality(
  assessment_id INT         NOT NULL,
  study_id      INT         NOT NULL,
  question_id   INT         NOT NULL,
  answer        VARCHAR(5)  NOT NULL,

  /* Keys */
  CONSTRAINT quality_pk PRIMARY KEY (assessment_id, study_id, question_id),
  FOREIGN KEY (assessment_id) REFERENCES assessments(assessment_id),
  FOREIGN KEY (study_id) REFERENCES studies(study_id),
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
  record_id     INT           NOT NULL,  AUTO_INCREMENT,
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
  FOREIGN KEY(assessment_id) REFERENCES assessments(assessment_id),
  FOREIGN KEY(study_id) REFERENCES studies(study_id),
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
