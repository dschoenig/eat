/* USE ONLY WITH SQLITE */

PRAGMA foreign_keys=OFF;

BEGIN TRANSACTION;

CREATE TABLE level_of_evidence(
  record_id     INTEGER   NOT NULL,
  assessment_id INTEGER   NOT NULL,
  study_id      INTEGER   NOT NULL,
  study_design  TEXT      NOT NULL,  -- type of study design
  res_context   TEXT      NOT NULL,  -- description of research context
  res_focus     TEXT      NOT NULL,  -- type of research focus
  res_question  TEXT      NOT NULL,  -- description of research question
  res_outcome   TEXT      NOT NULL,  -- description of research outcome
  loe_final     TEXT      NOT NULL,  -- LoE after downgrading
  loe_pre       TEXT      NOT NULL,  -- LoE based on study design
  points_p      INTEGER   NOT NULL,  -- possible quality points, calculated
  points_q      INTEGER   NOT NULL,  -- quality points achieved, calculated
  q_score       REAL      NOT NULL,  -- quality score as percentage, calculated
  downgrading   TEXT      NOT NULL,  -- downgrading based on quality score
  reviewed      TEXT      NOT NULL,  -- whether validity of databse entry has been reviewed

  /* Keys */
  PRIMARY KEY(record_id),
  FOREIGN KEY(assessment_id) REFERENCES assessments(assessment_id),
  FOREIGN KEY(study_id) REFERENCES studies(study_id),
  FOREIGN KEY(downgrading) REFERENCES adjustments(adjustment),
  FOREIGN KEY(study_design, loe_pre) REFERENCES study_designs(study_design, loe_pre),
  /* NB: for new records, the studies, assessor, and assements tables
  must be populated first */

  /* Checks */
  CHECK (res_focus IN ('Quantification', 'Valuation', 'Management', 'Governance')),
  CHECK (loe_final IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4')),
  CHECK (loe_pre IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4')),
  CHECK (points_q <= points_p),
  CONSTRAINT score_range CHECK (q_score>= 0 AND q_score <=100),
  CHECK (downgrading IN ('none', 'half a level', 'one level', 'one and a half levels',
                         'two levels', 'two and a half levels', 'three levels')),
  CHECK (reviewed IN ('yes', 'no'))
);

CREATE TABLE studies(
  study_id      INTEGER   NOT NULL,
  abbreviation  TEXT      NOT NULL,  -- formatted as AuthorYear
  authors       TEXT      NOT NULL,  -- study author(s)
  title         TEXT      NOT NULL,  -- study title
  year          INTEGER   NOT NULL,  -- publication year of study
  doi           TEXT,                -- DOI of study

  /* Keys */
  PRIMARY KEY(study_id)
);

CREATE TABLE assessors(
  assessor_id   INTEGER   NOT NULL,
  name          TEXT,                -- name of assessor
  email         TEXT      NOT NULL,  -- email contact of assessor

  /* Keys */
  PRIMARY KEY(assessor_id)
);

CREATE TABLE assessments(
  assessment_id INTEGER   NOT NULL,
  assessor_id   INTEGER   NOT NULL,
  date_entered  INTEGER   NOT NULL,  -- entry date of records in format YYYY-MM-DD
  source        TEXT,                -- published source of assessments

  /* Keys */
  PRIMARY KEY(assessment_id),
  FOREIGN KEY(assessor_id) REFERENCES assessors(assessor_id)
);

CREATE TABLE study_designs(
  study_design  TEXT      NOT NULL,  -- type of study design
  loe_pre       TEXT      NOT NULL,  -- LoE based on study design

  /* Keys */
  PRIMARY KEY(study_design, loe_pre),

  /* Checks */
  CHECK (loe_pre IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4'))
);

CREATE TABLE checklist(
  question_id   INTEGER   NOT NULL,  -- corresponds to number in checklist
  q_group       TEXT      NOT NULL,  -- corresponds to group in checklist
  q_subgroup    TEXT      NOT NULL,  -- corresponds to subgroup in checklist
  question      TEXT      NOT NULL,  -- question text
  description   TEXT,                -- question description

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

CREATE TABLE quality(
  assessment_id INTEGER   NOT NULL,
  study_id      INTEGER   NOT NULL,
  question_id   INTEGER   NOT NULL,
  answer        TEXT      NOT NULL,

  /* Keys */
  CONSTRAINT quality_pk PRIMARY KEY (assessment_id, study_id, question_id),
  FOREIGN KEY (assessment_id) REFERENCES assessments(assessment_id),
  FOREIGN KEY (study_id) REFERENCES studies(study_id),
  FOREIGN KEY (question_id) REFERENCES checklist(question_id),

  /* Checks */
  CHECK (answer IN ('yes', 'no', 'na'))
);

CREATE TABLE adjustments(
  adjustment_id INTEGER   NOT NULL,  -- adjustment identifier
  q_score_ub    INTEGER   NOT NULL,  -- upper bound (inclusive) of quality score range as percentage
  q_score_lb    INTEGER   NOT NULL,  -- lower bound (exclusive) of quality score range as percentage
  adjustment    TEXT      UNIQUE NOT NULL,  -- adjustment to LoE based on quality score range

  /* Keys */
  PRIMARY KEY (adjustment_id),

  /* Checks */
  CONSTRAINT score_range_ub CHECK (q_score_ub >= 0 AND q_score_ub <=100),
  CONSTRAINT score_range_lb CHECK (q_score_lb >= -1 AND q_score_lb <=100),
  CHECK (q_score_lb <= q_score_ub),
  CHECK (adjustment IN ('none', 'half a level', 'one level', 'one and a half levels',
                         'two levels', 'two and a half levels', 'three levels'))
);

CREATE TABLE downgrading(
  rule_id       INTEGER   NOT NULL,  -- rule identifier
  adjustment_id INTEGER   NOT NULL,  -- adjustment identifier
  loe_pre       TEXT      NOT NULL,  -- LoE based on study designs
  loe_final     TEXT      NOT NULL,  -- LoE after downgrading

  /* Keys */
  PRIMARY KEY (rule_id),
  FOREIGN KEY (adjustment_id) REFERENCES adjustments(adjustment_id),

  /* Checks */
  CHECK (loe_pre IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4')),
  CHECK (loe_final IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4'))
);

COMMIT;

PRAGMA foreign_keys=ON;
