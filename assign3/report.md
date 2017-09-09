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

Three commons errors that may occur when trying to complete this functionality manually are:

 1. Incorrectly graduating a student as there is nothing stopping the user from doing so
 2. Mistyping of key fields, such as student ids, as they are repetitively used
 3. Copy paste error from entering in the previous result

## SQL Code

```sql
CREATE OR REPLACE FUNCTION coursePass
    (In_sId int, In_cId char, In_year int, In_grade char, In_graduationDate date)
RETURNS boolean AS $$
DECLARE
    totalpts RECORD; 
BEGIN

    -- A student can get more than one grade for the same course, but each in a different year.
    INSERT INTO result VALUES(In_sId, In_cId, In_year, In_grade); -- The PK should prevent double ups

    -- When a student passes a course, she/he will have the points for this course added to
    -- her/his earned points. A student may earn the points for the same course only once.
    SELECT SUM(points) AS points INTO totalpts FROM
        (SELECT MIN(points) AS points FROM result NATURAL JOIN course
            WHERE result.sid = In_sId group by (result.cid, result.sid)) as p;

    UPDATE student SET pointsEarned = (totalpts.points) WHERE student.sid = In_sId;
    
    -- When a student has earned 360 points or more, she/he is recorded in the GRADUATE table.
    IF totalpts.points >= 360 THEN
        PERFORM * FROM graduate WHERE graduate.sid = In_sId;
        IF FOUND THEN
            UPDATE graduate SET graduationdate = In_graduationDate WHERE graduate.sid = In_sId;
        ELSE 
            INSERT INTO Graduate (sid, graduationdate) VALUES (In_sId, In_graduationDate);
        END IF;
    ELSE 
        DELETE FROM graduate WHERE graduate.sid = In_sId;
    END IF;

    return true;
END;
$$ LANGUAGE 'plpgsql';
```

## Test results

```
(Q2.Test0) SELECT COUNT (*) FROM RESULT; —- should give a number
 count 
-------
    50
(1 row)

(Q2.Test1) SELECT coursePass(5000, COMP205, 2014, A-, 2014-07-01); -- wrong student
psql:test/swen304_a3_q2_test.sql:5: ERROR:  insert or update on table "result" violates
    foreign key constraint "result_sid_fkey"
DETAIL:  Key (sid)=(5000) is not present in table "student".
CONTEXT:  SQL statement "INSERT INTO result VALUES(In_sId, In_cId, In_year, In_grade)"
PL/pgSQL function coursepass(integer,character,integer,character,date) line 7 at SQL statement
(Q2.Test2) SELECT coursePass(5003, SWEN205, 2014, A-, 2014-07-01); -- wrong course
psql:test/swen304_a3_q2_test.sql:8: ERROR:  insert or update on table "result" violates
    foreign key constraint "result_cid_fkey"
DETAIL:  Key (cid)=(SWEN205) is not present in table "course".
CONTEXT:  SQL statement "INSERT INTO result VALUES(In_sId, In_cId, In_year, In_grade)"
PL/pgSQL function coursepass(integer,character,integer,character,date) line 7 at SQL statement
(Q2.Test3) SELECT coursePass(5003, COMP202, 2014, B+, 2014-07-01); -- duplicate
psql:test/swen304_a3_q2_test.sql:11: ERROR:  duplicate key value violates unique
    constraint "result_pkey"
DETAIL:  Key (sid, cid, year)=(5003, COMP202, 2014) already exists.
CONTEXT:  SQL statement "INSERT INTO result VALUES(In_sId, In_cId, In_year, In_grade)"
PL/pgSQL function coursepass(integer,character,integer,character,date) line 7 at SQL statement
(Q2.Test4) SELECT coursePass(5003, COMP103, 2014, B-, 2014-07-01); -- now pass grade, was fail
 coursepass 
------------
 t
(1 row)

(Q2.Test4) SELECT pointsEarned FROM STUDENT WHERE sId = 5003; -- should return 150
 pointsearned 
--------------
          150
(1 row)

(Q2.Test5) SELECT coursePass(5003, ENGR101, 2014, A, 2014-07-01); -- now pass grade, was also pass
 coursepass 
------------
 t
(1 row)

(Q2.Test5) SELECT pointsEarned FROM STUDENT WHERE sId = 5003; -- should still return 150
 pointsearned 
--------------
          150
(1 row)

(Q2.Test6) SELECT coursePass(5003, MATH114, 2014, D, 2014-07-01); -- now fail grade, was pass
 coursepass 
------------
 t
(1 row)

(Q2.Test6) SELECT pointsEarned FROM STUDENT WHERE sId = 5003; -- should still return 150
 pointsearned 
--------------
          150
(1 row)

(Q2.Test7) SELECT coursePass(6006, COMP311, 2014, A+, 2014-07-01);
 coursepass 
------------
 t
(1 row)

(Q2.Test7) SELECT pointsEarned FROM STUDENT WHERE sId = 6006;
 pointsearned 
--------------
          465
(1 row)

(Q2.Test8) SELECT * FROM GRADUATE;
 sid  | graduationdate 
------+----------------
 7007 | 2013-11-30
 6006 | 2014-07-01
(2 rows)
```

# Question 3 - Database Access

## A)

### Database `ROLE`
<!-- What is a database ROLE? -->

### `ROLE-BASED ACCESS CONTROL`
<!-- What is ROLE-BASED ACCESS CONTROL? -->

### `PUBLIC` Role
<!-- What kind of a role is PUBLIC? -->

### `GRANT` Clause
<!-- What is the GRANT clause used for? -->

### `REVOKE` Clause
<!-- What is the REVOKE clause used for? -->

## B)

<!--
What is the purpose of executing the following SQL commands before users are permitted to use your database?

    GRANT CONNECT ON DATABASE <your_database_name> TO PUBLIC;
    GRANT SELECT, INSERT, DELETE, UPDATE ON MAJOR, STUDENT,
    COURSE, RESULT, GRADUATE TO PUBLIC;
-->

## C)
<!-- What happens if the RESULT table is missing in the SQL command in part b) -->

## D)
<!-- What happens if the UPDATE operation is missing in the SQL command in part b)? -->
