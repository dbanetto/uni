SELECT BankName, City FROM Banks 
WHERE BankName NOT IN 
    (SELECT DISTINCT BankName FROM Banks WHERE City = 'Chicago')
ORDER BY NoAccounts;
