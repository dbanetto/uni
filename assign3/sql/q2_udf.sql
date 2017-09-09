CREATE OR REPLACE FUNCTION coursePass(sid integer, cid unknown, year integer, grade unknown, grad unknown)
RETURNS boolean AS $$
DECLARE
    ssid ALIAS FOR sid;
    ccid ALIAS FOR cid;
    totalpts RECORD; 
    gradt TEXT;
    gradd DATE;
BEGIN
    gradt := grad::text;
    gradd := gradt::Date;
    cid := cid::varchar(7);
    grade := grade::varchar(2);

    -- A student can get more than one grade for the same course, but each in a different year.
    INSERT INTO result VALUES(sid, cid, year, grade); -- The PK should prevent double ups

    -- When a student passes a course, she/he will have the points for this course added to
    -- her/his earned points. A student may earn the points for the same course only once.
    SELECT SUM(points) AS points INTO totalpts FROM (SELECT MIN(points) AS points FROM result NATURAL JOIN course WHERE result.sid = ssid group by (result.cid, result.sid)) as p;
    UPDATE student SET pointsEarned = (totalpts.points) WHERE student.sid = ssid;
    
    -- When a student has earned 360 points or more, she/he is recorded in the GRADUATE table.
    IF totalpts.points >= 360 THEN
        PERFORM * FROM graduate WHERE graduate.sid = ssid;
        IF FOUND THEN
            UPDATE graduate SET graduationdate = gradd WHERE graduate.sid = ssid;
        ELSE 
            INSERT INTO Graduate (sid, graduationdate) VALUES (ssid, gradd);
        END IF;
    ELSE 
        PERFORM * FROM graduate WHERE graduate.sid = ssid;
        IF FOUND THEN
            DELETE FROM graduate WHERE graduate.sid = ssid;
        END IF;
    END IF;

    return true;
END;
$$ LANGUAGE 'plpgsql';
