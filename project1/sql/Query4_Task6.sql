SELECT Description, RobberId, Nickname 
FROM HasSkills NATURAL JOIN Robbers NATURAL JOIN Skills 
ORDER BY description;
