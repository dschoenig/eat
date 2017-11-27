/* USE ONLY WITH SQLITE */

PRAGMA foreign_keys=OFF;

BEGIN TRANSACTION;

CREATE TABLE loe(
  study_id      INTEGER   NOT NULL,
  study_design  TEXT      NOT NULL,
  context       TEXT      NOT NULL,
  focus         TEXT      NOT NULL,
  question      TEXT      NOT NULL,
  outcome       TEXT      NOT NULL,
  loe_final     TEXT      NOT NULL,  -- LoE after downgrading
  loe_pre       TEXT      NOT NULL,  -- LoE based on study design
  points_p      INTEGER   NOT NULL,  -- possible quality points
  points_q      INTEGER   NOT NULL,  -- quality points achieved
  q_score       INTEGER   NOT NULL,  -- quality score as percentage
  dgrade        TEXT      NOT NULL,  -- downgrading based on quality score
  assessor_id   INTEGER   NOT NULL,  -- assessor_id
  reviewed      TEXT      NOT NULL,  -- whether databse entry has been reviewed

  /* Keys */
  PRIMARY KEY(study_id),
  FOREIGN KEY(assessor_id) REFERENCES assesors(assessor_id),
  FOREIGN KEY(study_id) REFERENCES studies(study_id),
  /* NB for new records the studies table must be populated first */

  /* Checks */
  CHECK (study_design IN ('Systematic review', 'Conventional review',
                         'Case control', 'Before-after control-impact',
                         'Multiple lines of moderate evidence', 'Inferential',
                         'Descriptive', 'Multiple lines of weak evidence',
                         'Expert opinion', 'Mechanism-based')),
  CHECK (focus IN ('Quantification', 'Valuation', 'Management', 'Governance')),
  CHECK (loe_final IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4')),
  CHECK (loe_pre IN ('LoE1a', 'LoE1b', 'LoE2a', 'LoE2b', 'LoE3a', 'LoE3b', 'LoE3c', 'LoE4')),
  CHECK (points_q <= points_p),
  CONSTRAINT score_range CHECK (q_score>= 0 AND q_score <=100),
  CHECK (dgrade IN ('half a level', 'one level', 'one and a half levels',
                    'two levels', 'two and a half levels', 'three levels')),
  CHECK (reviewed IN ('yes', 'no')),
  CONSTRAINT study_design_loe_pre  -- possible matchings between design and loe_pre
       CHECK ((study_design = 'Systematic review' AND loe_pre = 'LoE1a') OR
              (study_design = 'Conventional review' AND loe_pre = 'LoE1b') OR
              (study_design = 'Case control' AND loe_pre = 'LoE2a') OR
              (study_design = 'Before-after control-impact' AND loe_pre = 'LoE2a') OR
              (study_design = 'Multiple lines of moderate evidence' AND loe_pre = 'LoE2b') OR
              (study_design = 'Observational (Inferential)' AND loe_pre = 'LoE3a') OR
              (study_design = 'Observational (Descriptive)' AND loe_pre = 'LoE3b') OR
              (study_design = 'Multiple lines of weak evidence' AND loe_pre = 'LoE3c') OR
              (study_design = 'Expert opinion' AND loe_pre = 'LoE4') OR
              (study_design = 'Mechanism-based reasoning' AND loe_pre = 'LoE4'))
);

CREATE TABLE assesors(
  assessor_id   INTEGER   NOT NULL,
  name          TEXT,                -- name of assesor
  source        TEXT,                -- published source of assessments
  email         TEXT      NOT NULL,  -- email contact of assessor

  /* Keys */
  PRIMARY KEY(assessor_id)
);

CREATE TABLE studies(
  study_id      INTEGER   NOT NULL,
  shorthand     TEXT      NOT NULL,  -- formatted as AuthorYear
  authors       TEXT      NOT NULL,
  title         TEXT      NOT NULL,
  year          INTEGER   NOT NULL,
  doi           TEXT,
  bibtex        TEXT      NOT NULL,  -- citation in bibtex format

  /* Keys */
  PRIMARY KEY(study_id)
);

CREATE TABLE checklist(
  question_id   INTEGER   NOT NULL,  -- corresponds to number in checklist
  q_group       TEXT      NOT NULL,
  q_subgroup    TEXT      NOT NULL,
  question      TEXT      NOT NULL,
  description   TEXT      NOT NULL,

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
  study_id      INTEGER   NOT NULL,
  question_id   INTEGER   NOT NULL,
  answer        TEXT,

  /* Keys */
  CONSTRAINT quality_pk PRIMARY KEY (study_id, question_id),
  FOREIGN KEY (study_id) REFERENCES studies(study_id),
  FOREIGN KEY (question_id) REFERENCES checklist(question_id),

  /* Checks */
  CHECK (answer IN ("yes", "no", NULL))
);

COMMIT;

PRAGMA foreign_keys=ON;
