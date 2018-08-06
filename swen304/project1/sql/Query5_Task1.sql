
--- Step-Wise

-- setup
CREATE VIEW ParticipatedIn AS 
    SELECT RobberId, COUNT(RobberId) as participated, SUM("Share") as earnings FROM accomplices 
    GROUP BY robberid;

CREATE VIEW AvgParticipation AS
    SELECT AVG(Participated) as AvgPart FROM ParticipatedIn;


CREATE VIEW AboveAvgParticipation AS
    SELECT Nickname FROM 
    Robbers NATURAL JOIN ParticipatedIn NATURAL JOIN AvgParticipation
    WHERE (AvgPart < Participated) AND (NoYears = 0)
    ORDER BY Earnings DESC;

-- query
SELECT * FROM AboveAvgParticipation;

-- clean up
DROP VIEW AboveAvgParticipation;
DROP VIEW AvgParticipation;
DROP VIEW ParticipatedIn;


--- Nested Queries

SELECT Nickname FROM
Robbers NATURAL JOIN 
    (SELECT RobberId, COUNT(RobberId) as Part, SUM("Share") as Earn FROM Accomplices
        GROUP BY RobberID) AS P
    NATURAL JOIN
    (SELECT AVG(Pa) AS PartAvg FROM 
        (SELECT RobberId, COUNT(RobberId) as Pa FROM Accomplices
            GROUP BY RobberID) AS PP
    ) AS AV
WHERE (PartAvg < Part) AND (NoYears = 0)
ORDER BY Earn DESC;
