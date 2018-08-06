\qecho (Q2.Test0) SELECT COUNT (*) FROM RESULT; —- should give a number
SELECT COUNT (*) FROM RESULT;

\qecho (Q2.Test1) SELECT coursePass(5000, 'COMP205', 2014, 'A-', '2014-07-01'); -- wrong student
SELECT coursePass(5000, 'COMP205', 2014, 'A-', '2014-07-01'); 

\qecho (Q2.Test2) SELECT coursePass(5003, 'SWEN205', 2014, 'A-', '2014-07-01'); -- wrong course
SELECT coursePass(5003, 'SWEN205', 2014, 'A-', '2014-07-01');

\qecho (Q2.Test3) SELECT coursePass(5003, 'COMP202', 2014, 'B+', '2014-07-01'); -- duplicate
SELECT coursePass(5003, 'COMP202', 2014, 'B+', '2014-07-01');

\qecho (Q2.Test4) SELECT coursePass(5003, 'COMP103', 2014, 'B-', '2014-07-01'); -- now pass grade, was fail
SELECT coursePass(5003, 'COMP103', 2014, 'B-', '2014-07-01');

\qecho (Q2.Test4) SELECT pointsEarned FROM STUDENT WHERE sId = 5003; -- should return 150
SELECT pointsEarned FROM STUDENT WHERE sId = 5003;

\qecho (Q2.Test5) SELECT coursePass(5003, 'ENGR101', 2014, 'A', '2014-07-01'); -- now pass grade, was also pass
SELECT coursePass(5003, 'ENGR101', 2014, 'A', '2014-07-01');

\qecho (Q2.Test5) SELECT pointsEarned FROM STUDENT WHERE sId = 5003; -- should still return 150
SELECT pointsEarned FROM STUDENT WHERE sId = 5003;

\qecho (Q2.Test6) SELECT coursePass(5003, 'MATH114', 2014, 'D', '2014-07-01'); -- now fail grade, was pass
SELECT coursePass(5003, 'MATH114', 2014, 'D', '2014-07-01');

\qecho (Q2.Test6) SELECT pointsEarned FROM STUDENT WHERE sId = 5003; -- should still return 150
SELECT pointsEarned FROM STUDENT WHERE sId = 5003;

\qecho (Q2.Test7) SELECT coursePass(6006, 'COMP311', 2014, 'A+', '2014-07-01');
SELECT coursePass(6006, 'COMP311', 2014, 'A+', '2014-07-01');

\qecho (Q2.Test7) SELECT pointsEarned FROM STUDENT WHERE sId = 6006;
SELECT pointsEarned FROM STUDENT WHERE sId = 6006;

\qecho (Q2.Test8) SELECT * FROM GRADUATE;
SELECT * FROM GRADUATE;