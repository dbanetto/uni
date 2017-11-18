SELECT RobberId, Nickname, (Age - NoYears) AS NotInPrison
FROM Robbers WHERE (NoYears * 2 >= Age);
