\qecho (Q1.Test0) SELECT COUNT (*) FROM STUDENT; —- should give a number
SELECT COUNT (*) FROM STUDENT;

\qecho (Q1.Test1) INSERT INTO MAJOR VALUES('SE', 'Software Engineering'); —- should succeed
INSERT INTO MAJOR VALUES('SE', 'Software Engineering');

\qecho (Q1.Test2) INSERT INTO STUDENT VALUES(5001, 'Lisa', 'Simpson', 'SE', 180); —- should succeed
INSERT INTO STUDENT VALUES(5001, 'Lisa', 'Simpson', 'SE', 180);

\qecho (Q1.Test3) INSERT INTO STUDENT VALUES(5002, 'Bart', 'Simpson', 'Bio', 90); —- should fail
INSERT INTO STUDENT VALUES(5002, 'Bart', 'Simpson', 'Bio', 90);

\qecho (Q1.Test4) UPDATE STUDENT SET pointsEarned = pointsEarned + 15 WHERE sId = 5001; —- should succeed
UPDATE STUDENT SET pointsEarned = pointsEarned + 15 WHERE sId = 5001;

\qecho (Q1.Test4) SELECT pointsEarned FROM STUDENT WHERE sId = 5001; -- should return 195
SELECT pointsEarned FROM STUDENT WHERE sId = 5001;

\qecho (Q1.Test5) UPDATE STUDENT SET mCode = 'Bio' WHERE sId = 5001; —- should fail
UPDATE STUDENT SET mCode = 'Bio' WHERE sId = 5001;

\qecho (Q1.Test6) INSERT INTO MAJOR VALUES('OR', 'Operations Research'); —- should succeed
INSERT INTO MAJOR VALUES('OR', 'Operations Research');

\qecho (Q1.Test6) DELETE FROM MAJOR WHERE mCode = 'OR'; —- should succeed
DELETE FROM MAJOR WHERE mCode = 'OR';

\qecho (Q1.Test6) SELECT * FROM MAJOR; —- there should be no OR
SELECT * FROM MAJOR;

\qecho (Q1.Test7) DELETE FROM MAJOR WHERE mCode = 'SE'; —- should fail
DELETE FROM MAJOR WHERE mCode = 'SE';

\qecho (Q1.Test8) INSERT INTO MAJOR VALUES('NE', 'Networking'); —- should succeed
INSERT INTO MAJOR VALUES('NE', 'Networking');

\qecho (Q1.Test8) UPDATE MAJOR SET name = 'Network Engineering' WHERE mCode = 'NE'; -- should succeed
UPDATE MAJOR SET name = 'Network Engineering' WHERE mCode = 'NE';

\qecho (Q1.Test8) SELECT name FROM MAJOR WHERE mCode = 'NE'; -- should return 'Network Engineering'
SELECT name FROM MAJOR WHERE mCode = 'NE';

\qecho (Q1.Test9) UPDATE MAJOR SET mCode = 'SoftE' WHERE mCode = 'SE';
UPDATE MAJOR SET mCode = 'SoftE' WHERE mCode = 'SE';

\qecho (Q1.Test9) SELECT mCode FROM STUDENT WHERE sId = 5001;
SELECT mCode FROM STUDENT WHERE sId = 5001;
