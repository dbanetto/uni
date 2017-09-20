CREATE OR REPLACE FUNCTION coursePass (In_sId int, In_cId char, In_year int, In_grade char, In_graduationDate date)
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
           WHERE result.sid = In_sId AND grade IN ('C-', 'C', 'C+', 'B-', 'B', 'B+', 'A-', 'A', 'A+')
            group by (result.cid, result.sid)) as p;

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
