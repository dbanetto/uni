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
