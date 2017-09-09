% SWEN304 - Assignment 3
% David Barnett (300313764)

# Question 1 - Triggers

## SQL Code

```sql
CREATE OR REPLACE FUNCTION check_major()
RETURNS trigger AS $$
BEGIN
    PERFORM * FROM Major WHERE mCode = NEW.mCode;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'Cannot set student major to non-existing %', NEW.mCode;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE 'plpgsql';


CREATE TRIGGER fk_student_major_insert
BEFORE INSERT OR UPDATE ON STUDENT
FOR EACH ROW
EXECUTE PROCEDURE check_major();

CREATE OR REPLACE FUNCTION ensure_major_empty()
RETURNS trigger AS $$
BEGIN
    PERFORM * FROM Student WHERE mCode = OLD.mCode;
    IF FOUND THEN
        RAISE EXCEPTION 'Cannot delete % while there are some students with that major', OLD.mCode;
    END IF;
    RETURN OLD;
END;
$$ LANGUAGE 'plpgsql';

CREATE TRIGGER fk_student_major_delete
BEFORE DELETE OR UPDATE OF mCode ON Major
FOR EACH ROW
EXECUTE PROCEDURE ensure_major_empty();
```

## Log of Test

```
(Q1.Test0) SELECT COUNT (*) FROM STUDENT; —- should give a number
 count
-------
     9
(1 row)

(Q1.Test1) INSERT INTO MAJOR VALUES(SE, Software Engineering); —- should succeed
INSERT 0 1
(Q1.Test2) INSERT INTO STUDENT VALUES(5001, Lisa, Simpson, SE, 180); —- should succeed
INSERT 0 1
(Q1.Test3) INSERT INTO STUDENT VALUES(5002, Bart, Simpson, Bio, 90); —- should fail
psql:test/swen304_a3_q1_test.sql:11: ERROR:  Cannot set student major to non-existing Bio
CONTEXT:  PL/pgSQL function check_major() line 5 at RAISE
(Q1.Test4) UPDATE STUDENT SET pointsEarned = pointsEarned + 15 WHERE sId = 5001; —- should succeed
UPDATE 1
(Q1.Test4) SELECT pointsEarned FROM STUDENT WHERE sId = 5001; -- should return 195
 pointsearned
--------------
          195
(1 row)

(Q1.Test5) UPDATE STUDENT SET mCode = Bio WHERE sId = 5001; —- should fail
psql:test/swen304_a3_q1_test.sql:20: ERROR:  Cannot set student major to non-existing Bio
CONTEXT:  PL/pgSQL function check_major() line 5 at RAISE
(Q1.Test6) INSERT INTO MAJOR VALUES(OR, Operations Research); —- should succeed
INSERT 0 1
(Q1.Test6) DELETE FROM MAJOR WHERE mCode = OR; —- should succeed
DELETE 1
(Q1.Test6) SELECT * FROM MAJOR; —- there should be no OR
 mcode  |         name
--------+----------------------
 Stat   | Statistics
 CS     | Computer Science
 Math   | Mathematics
 SE     | Software Engineering
(4 rows)

(Q1.Test7) DELETE FROM MAJOR WHERE mCode = SE; —- should fail
psql:test/swen304_a3_q1_test.sql:32: ERROR:  Cannot delete SE     while there are some
    students with that major
CONTEXT:  PL/pgSQL function ensure_major_empty() line 5 at RAISE
(Q1.Test8) INSERT INTO MAJOR VALUES(NE, Networking); —- should succeed
INSERT 0 1
(Q1.Test8) UPDATE MAJOR SET name = Network Engineering WHERE mCode = NE; -- should succeed
UPDATE 1
(Q1.Test8) SELECT name FROM MAJOR WHERE mCode = NE; -- should return Network Engineering
        name
---------------------
 Network Engineering
(1 row)

(Q1.Test9) UPDATE MAJOR SET mCode = SoftE WHERE mCode = SE;
psql:test/swen304_a3_q1_test.sql:44: ERROR:  Cannot delete SE     while there are some
    students with that major
CONTEXT:  PL/pgSQL function ensure_major_empty() line 5 at RAISE
(Q1.Test9) SELECT mCode FROM STUDENT WHERE sId = 5001;
 mcode
--------
 SE
(1 row)
```

## B)

# Question 2 - User defined functions

## A)

## B)

## C)

# Question 3 - Database Access

## A)

## B)

## C)

## D)
