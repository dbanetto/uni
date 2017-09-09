CREATE OR REPLACE FUNCTION coursePass(sid integer, cid unknown, year integer, grade unknown, grad unknown)
RETURNS record AS $$
BEGIN
    grad := (grad::text)::Date;
    cid := cid::varchar(7);
    grade := grade::varchar(2);
    -- A student can get more than one grade for the same course, but each in a different year.

    -- When a student passes a course, she/he will have the points for this course added to
    -- her/his earned points. A student may earn the points for the same course only once.

    -- When a student has earned 360 points or more, she/he is recorded in the GRADUATE table.

    return (sid, cid, year, grade, grad);
END;
$$ LANGUAGE 'plpgsql';
