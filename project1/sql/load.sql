\COPY Banks FROM data/banks_17.data;
\COPY Robberies FROM data/robberies_17.data;
\COPY Plans FROM data/plans_17.data;
\COPY Robbers(Nickname, Age, NoYears) FROM data/robbers_17.data;

CREATE TABLE SkillsRaw (
    Nickname text NOT NULL,
    Description text NOT NULL,
    Preference integer NOT NULL,
    Grade text NOT NULL
);

\COPY SkillsRaw FROM data/hasskills_17.data

INSERT INTO Skills (Description)
    (SELECT DISTINCT (Description) FROM SkillsRaw);

INSERT INTO HasSkills (RobberId, SkillId, Preference, Grade)
    (SELECT RobberId, SkillId, Preference, TRIM(Grade)::Grades
        FROM skillsraw NATURAL JOIN robbers NATURAL JOIN skills);

DROP TABLE SkillsRaw;

CREATE TABLE HasAccountsRaw (
    Nickname text NOT NULL,
    BankName text NOT NULL,
    City text NOT NULL
);

\COPY HasAccountsRaw FROM data/hasaccounts_17.data

INSERT INTO HasAccounts (RobberId, BankName, City)
    (SELECT RobberId, BankName, City
        FROM HasAccountsRaw NATURAL JOIN Robbers);

DROP TABLE HasAccountsRaw;

CREATE TABLE AccomplicesRaw (
    Nickname text NOT NULL,
    BankName text NOT NULL,
    City text NOT NULL,
    RobberyDate date NOT NULL,
    "Share" money NOT NULL
);

\COPY AccomplicesRaw FROM data/accomplices_17.data

INSERT INTO Accomplices (RobberId, BankName, City, RobberyDate, "Share")
    (SELECT RobberId, BankName, City, RobberyDate, "Share" FROM
    AccomplicesRaw NATURAL JOIN Robbers);

DROP TABLE AccomplicesRaw;
