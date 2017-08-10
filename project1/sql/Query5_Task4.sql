--- Step-wise

-- setup

CREATE VIEW PlannedRobberies AS
    SELECT BankName, City, Security, PlannedDate FROM
    Banks NATURAL JOIN Plans
    WHERE PlannedDate < (NOW() + interval '1 year')::Date AND
          PlannedDate > NOW();

CREATE VIEW RecentRobberies AS
    SELECT BankName, City, Security, "Date" FROM
    Banks NATURAL JOIN Robberies
    WHERE "Date" > (NOW() - interval '1 year')::Date AND
          "Date" < NOW()::Date;

CREATE VIEW SoonVictims AS
    SELECT P.BankName, P.City, P.Security FROM
    PlannedRobberies as P LEFT JOIN RecentRobberies as R
    ON R.Bankname = P.Bankname AND R.City = P.City;

-- query
SELECT * FROM SoonVictims;

-- SELECT * FROM 

-- cleanup
DROP VIEW SoonVictims;
DROP VIEW PlannedRobberies;
DROP VIEW RecentRobberies;

--- Nested Query


SELECT P.BankName, P.City, P.Security FROM
    (SELECT BankName, City, Security, PlannedDate FROM
    Banks NATURAL JOIN Plans
    WHERE PlannedDate < (NOW() + interval '1 year')::Date AND
    PlannedDate > NOW()) AS P
LEFT JOIN
    (SELECT BankName, City, Security, "Date" FROM
    Banks NATURAL JOIN Robberies
    WHERE "Date" > (NOW() - interval '1 year')::Date AND
          "Date" < NOW()::Date) AS R
    ON R.Bankname = P.Bankname AND R.City = P.City;
