CREATE TABLE Banks (
    BankName text NOT NULL,
    City text NOT NULL,
    NoAccounts integer CHECK (NoAccounts >= 0) DEFAULT 0,
    Security text DEFAULT 'weak',

    PRIMARY KEY (BankName, City)
);

CREATE TABLE Robberies (
    BankName text NOT NULL,
    City text NOT NULL,
    "Date" date DEFAULT TODAY(),
    Amount money NOT NULL,

    CONSTRAINT fk_robberies_bank FOREIGN KEY (BankName, City) 
    REFERENCES Banks(BankName, City)
);

CREATE TABLE Plans (
    BankName text NOT NULL,
    City text NOT NULL,
    PlannedDate date,
    NoRobbers integer CHECK (NoRobbers >= 0),

    CONSTRAINT fk_plan_bank FOREIGN KEY (BankName, City) 
    REFERENCES Banks(BankName, City)
);

CREATE TABLE Robbers (
    RobberId SERIAL PRIMARY KEY,
    Nickname text,
    Age integer,
    NoYears integer,

    CONSTRAINT non_neg_age CHECK (Age >= 0 AND NoYears >= 0),
    CONSTRAINT age_gt_noyears CHECK (Age >= NoYears)
);

CREATE TABLE Skills (
    SkillId integer PRIMARY KEY,
    Description text UNIQUE
);

CREATE TABLE HasSkills (
    RobberId integer REFERENCES Robbers(RobberId),
    SkillId integer REFERENCES Skills(SkillId),
    Preference text,
    Grade text
);

CREATE TABLE HasAccounts (
    RobberId integer REFERENCES Robbers(RobberId),
    BankName text,
    City text,

    CONSTRAINT fk_account_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City)
);


CREATE TABLE Accomplices (
    RobberId integer REFERENCES Robbers(RobberId),
    BankName text,
    City text,
    RobberyDate date,
    "Share" money,

    CONSTRAINT fk_accomplice_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City)
);
