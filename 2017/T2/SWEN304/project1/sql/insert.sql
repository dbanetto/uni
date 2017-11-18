INSERT INTO Banks (BankName, City, NoAccounts, Security)
    VALUES ('Loanshark Bank', 'Evanston', 100, 'very good'::SecurityLevel);

INSERT INTO Banks (BankName, City, NoAccounts, Security)
    VALUES ('Loanshark Bank', 'Evanston', -5, 'excellent'::SecurityLevel);

INSERT INTO Banks (BankName, City, NoAccounts, Security)
    VALUES ('Loanshark Bank', 'Evanston', 100, 'poor'::SecurityLevel);

INSERT INTO Skills (SkillId, Description)
    VALUES (20, 'Guarding');

INSERT INTO Robbers (RobberId, Nickname, Age, NoYears)
    VALUES (1, 'Shotgun', 70, 0);

INSERT INTO Robbers (RobberId, Nickname, Age, NoYears)
    VALUES (333, 'Jail Mouse', 25, 35);


INSERT INTO HasSkills (RobberId, SkillId, Preference, Grade)
    VALUES (333, 1, 1, 'B-');

INSERT INTO HasSkills (RobberId, SkillId, Preference, Grade)
    VALUES (3, 20, 3, 'B+');

INSERT INTO HasSkills (RobberId, SkillId, Preference, Grade)
    VALUES (1, 7, 1, 'A+');

INSERT INTO HasSkills (RobberId, SkillId, Preference, Grade)
    VALUES (1, 2, 0, 'A');

INSERT INTO Robberies
    VALUES ('NXP Bank', 'Chicago', '2009-01-08', 1000);


DELETE FROM Banks WHERE 
    BankName = 'PickPocket Bank' AND
    City = 'Evanston' AND
    NoAccounts = 2000 AND
    Security = 'very good';

DELETE FROM Banks WHERE 
    BankName = 'Outside Bank' AND
    City = 'Chicago' AND
    NoAccounts = 5000 AND
    Security = 'good';

DELETE FROM Robbers WHERE 
    RobberId = 1 AND
    Nickname = 'Al Capone' AND
    Age = 31 AND
    NoYears = 2;

DELETE FROM Skills WHERE 
    SkillId = 1 AND
    Description = 'Driving';

