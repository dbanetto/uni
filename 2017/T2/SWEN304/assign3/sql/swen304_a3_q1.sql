CREATE TABLE MAJOR(
	mCode char(6) PRIMARY KEY,
	name varchar
	);

CREATE TABLE STUDENT(
	sId int PRIMARY KEY,
	firstName varchar,
	lastName varchar,
	mCode char(6) NOT NULL,
	pointsEarned smallint DEFAULT 0
	);

INSERT INTO MAJOR VALUES('Stat','Statistics');
INSERT INTO MAJOR VALUES('CS','Computer Science');
INSERT INTO MAJOR VALUES('Math','Mathematics');

INSERT INTO STUDENT VALUES(321,'Megan','Haywood','Stat',0);
INSERT INTO STUDENT VALUES(992,'Richard','Clark','Stat',0);
INSERT INTO STUDENT VALUES(5003,'Sue','Leslie','CS',120);
INSERT INTO STUDENT VALUES(104,'Xi','Zhang','CS',120);
INSERT INTO STUDENT VALUES(105,'Neil','Nickson','CS',90);
INSERT INTO STUDENT VALUES(6006,'Maria','Cordes','CS',345);
INSERT INTO STUDENT VALUES(7007,'James','Bond','CS',390);
INSERT INTO STUDENT VALUES(108,'Steven','Atkin','Math',60);
INSERT INTO STUDENT VALUES(109,'Colin','Archer','Math',30);