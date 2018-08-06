SELECT BankName, City FROM Banks
NATURAL JOIN Robberies
ORDER BY "Date" LIMIT 1;
