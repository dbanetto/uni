--- Step-wise

--setup

CREATE VIEW RobberSkills AS
    SELECT RobberId, Nickname, Description FROM
    Robbers NATURAL JOIN HasSkills NATURAL JOIN Skills;

CREATE VIEW RobberySecurity AS
    SELECT RobberId, Security FROM
    Robbers NATURAL JOIN Accomplices NATURAL JOIN Banks;


CREATE VIEW SkillsUsed AS
    SELECT Security, Description, Nickname FROM
    RobberSkills NATURAL JOIN RobberySecurity;

-- query

--SELECT * FROM RobberSkills;
--SELECT * FROM RobberySecurity;
SELECT DISTINCT * FROM SkillsUsed;

-- cleanup

DROP VIEW SkillsUsed;
DROP VIEW RobberySecurity;
DROP VIEW RobberSkills;


--- Nested queries

SELECT DISTINCT Security, Description, Nickname FROM
    Robbers NATURAL JOIN HasSkills NATURAL JOIN Skills
    NATURAL JOIN Accomplices NATURAL JOIN Banks;
