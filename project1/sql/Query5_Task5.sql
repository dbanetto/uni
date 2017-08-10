--- Step-wise

-- setup
CREATE VIEW AvgShareByCity AS
    SELECT City, AVG("Share"::numeric) AS AvgShare FROM
        Accomplices
        GROUP BY City;

CREATE VIEW DistrictSummary AS
    SELECT 'Other'::text AS City, AVG(AvgShare) AS DistrictShare FROM
    AvgShareByCity
    GROUP BY City
    HAVING (City != 'Chicago');

CREATE VIEW chicagosummary AS
    SELECT City, AVG(AvgShare) AS DistrictShare FROM
    AvgShareByCity
    GROUP BY City
    HAVING (City = 'Chicago');

CREATE VIEW Summary AS
    SELECT * FROM DistrictSummary
    UNION
    SELECT * FROM ChicagoSummary;

-- query

SELECT * FROM AvgShareByCity;
SELECT * FROM DistrictSummary;
SELECT * FROM ChicagoSummary;
SELECT * FROM Summary;

-- cleaup
DROP VIEW Summary;
DROP VIEW DistrictSummary;
DROP VIEW ChicagoSummary;
DROP VIEW AvgShareByCity;

--- Nested queries


