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

    CONSTRAINT fk_robberies_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City) -- ON DELETE
);

CREATE TABLE Plans (
    BankName text NOT NULL,
    City text NOT NULL,
    PlannedDate date NOT NULL,
    NoRobbers integer NOT NULL CHECK (NoRobbers >= 0),

    CONSTRAINT fk_plan_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City) -- ON DELETE
);

CREATE TABLE Robbers (
    RobberId SERIAL PRIMARY KEY,
    Nickname text NOT NULL,
    Age integer NOT NULL,
    NoYears integer NOT NULL,

    CONSTRAINT non_neg_age CHECK (Age >= 0 AND NoYears >= 0),
    CONSTRAINT age_gt_noyears CHECK (Age >= NoYears)
);


CREATE TABLE Skills (
    SkillId serial PRIMARY KEY,
    Description text NOT NULL UNIQUE
);

CREATE TYPE Grades AS ENUM ('C-', 'C', 'C+', 'B-', 'B', 'B+', 'A-', 'A', 'A+');

CREATE TABLE HasSkills (
    RobberId integer NOT NULL REFERENCES Robbers(RobberId),
    SkillId integer NOT NULL REFERENCES Skills(SkillId),
    Preference integer NOT NULL,
    Grade Grades NOT NULL,

    CONSTRAINT pref_positive CHECK (Preference > 0),
    CONSTRAINT unique_robber_pref UNIQUE (RobberId, Preference)
);

CREATE TABLE HasAccounts (
    RobberId integer NOT NULL REFERENCES Robbers(RobberId), -- ON DELETE
    BankName text NOT NULL,
    City text NOT NULL,

    CONSTRAINT fk_account_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City) -- ON DELETE
);


CREATE TABLE Accomplices (
    RobberId integer REFERENCES Robbers(RobberId),
    BankName text NOT NULL,
    City text NOT NULL,
    RobberyDate date NOT NULL,
    "Share" money NOT NULL,

    CONSTRAINT fk_accomplice_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City) -- ON DELETE
);
