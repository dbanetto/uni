SELECT * FROM 
    (SELECT RobberId, NickName, SUM("Share") AS Earnings 
     FROM Robbers NATURAL JOIN Accomplices 
     GROUP BY RobberId) AS RobberEarnings 
WHERE Earnings > '30000'::money 
ORDER BY Earnings DESC;
