--- Step-wise

CREATE VIEW BankRobberies AS
    SELECT Security, Count(*) as TotalRobberies, AVG(Amount::numeric) as AvgAmount
    FROM robberies NATURAL JOIN Banks GROUP BY Security;

SELECT * FROM BankRobberies;

-- Clean up
DROP VIEW BankRobberies;

-- Nested-queries

SELECT Security, Count(*) as TotalRobberies, AVG(Amount::numeric) as AvgAmount
    FROM robberies NATURAL JOIN Banks GROUP BY Security;
