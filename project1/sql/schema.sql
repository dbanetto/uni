CREATE TYPE SecurityLevel AS ENUM ('weak', 'good', 'very good', 'excellent');

CREATE TABLE Banks (
    BankName text NOT NULL,
    City text NOT NULL,
    NoAccounts integer DEFAULT 0,
    Security SecurityLevel NOT NULL DEFAULT 'weak',

    PRIMARY KEY (BankName, City),
    CONSTRAINT no_neg_accounts CHECK (NoAccounts >= 0)
);

CREATE TABLE Robberies (
    BankName text NOT NULL,
    City text NOT NULL,
    "Date" date NOT NULL,
    Amount money NOT NULL CHECK (Amount >= '0.0'::money),

    CONSTRAINT pk_robberies PRIMARY KEY (BankName, City, "Date"),

    CONSTRAINT fk_robberies_bank
    FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City)
        ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE Plans (
    BankName text NOT NULL,
    City text NOT NULL,
    PlannedDate date NOT NULL,
    NoRobbers integer NOT NULL CHECK (NoRobbers >= 0),

    CONSTRAINT pk_plans PRIMARY KEY
        (BankName, City, PlannedDate, NoRobbers),

    CONSTRAINT fk_plan_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City)
        ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE Robbers (
    RobberId SERIAL PRIMARY KEY,
    Nickname text NOT NULL,
    Age integer,
    NoYears integer,

    CONSTRAINT non_neg_age CHECK (Age >= 0 AND NoYears >= 0),
    CONSTRAINT age_gt_noyears CHECK (Age >= NoYears)
);


CREATE TABLE Skills (
    SkillId serial PRIMARY KEY,
    Description text NOT NULL UNIQUE
);

CREATE TYPE Grades AS ENUM ('C-', 'C', 'C+', 'B-', 'B', 'B+', 'A-', 'A', 'A+');

CREATE TABLE HasSkills (
    RobberId integer NOT NULL REFERENCES Robbers(RobberId)
        ON DELETE CASCADE ON UPDATE CASCADE,
    SkillId integer NOT NULL REFERENCES Skills(SkillId)
        ON DELETE CASCADE ON UPDATE CASCADE,
    Preference integer NOT NULL,
    Grade Grades NOT NULL,

    CONSTRAINT pref_positive CHECK (Preference > 0),
    CONSTRAINT pk_has_skill PRIMARY KEY  (RobberId, SkillId, Preference)

);

CREATE TABLE HasAccounts (
    RobberId integer NOT NULL REFERENCES Robbers(RobberId)
        ON DELETE CASCADE ON UPDATE CASCADE,
    BankName text NOT NULL,
    City text NOT NULL,

    CONSTRAINT pk_has_accounts PRIMARY KEY (RobberId, BankName, City),
    CONSTRAINT fk_account_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City)
        ON DELETE CASCADE ON UPDATE CASCADE
);


CREATE TABLE Accomplices (
    RobberId integer REFERENCES Robbers(RobberId)
        ON UPDATE CASCADE ON DELETE CASCADE,
    BankName text NOT NULL,
    City text NOT NULL,
    RobberyDate date NOT NULL,
    "Share" money NOT NULL,

    CONSTRAINT pk_accomplices PRIMARY KEY
        (RobberId, BankName, City, RobberyDate),

    CONSTRAINT non_neg_share CHECK ("Share" >= '0'::money),
    CONSTRAINT fk_accomplice_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City)
        ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT fk_accomplice_robbery FOREIGN KEY (BankName, City, RobberyDate)
    REFERENCES Robberies(BankName, City, "Date")
        ON UPDATE CASCADE ON DELETE CASCADE
);
