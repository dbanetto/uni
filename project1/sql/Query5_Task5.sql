--- Step-wise

-- setup
CREATE VIEW AvgShareByCity AS
    SELECT City, AVG("Share"::numeric) AS AvgShare FROM
        Accomplices
        GROUP BY City;

CREATE VIEW DistrictSummary AS
    SELECT City, AVG(AvgShare) AS DistrictShare FROM
    AvgShareByCity
    GROUP BY City
    HAVING (City != 'Chicago')
    ORDER BY DistrictShare LIMIT 1;

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

SELECT * FROM
    (SELECT City, AVG(AvgShare) AS DistrictShare
        FROM (SELECT City, AVG("Share"::numeric) AS AvgShare FROM
            Accomplices
            GROUP BY City) AS AvgShareCity
    GROUP BY City
    HAVING (City != 'Chicago')
    ORDER BY DistrictShare
    LIMIT 1) AS District
    UNION
    SELECT * FROM
    (SELECT City, AVG("Share"::numeric) AS DistrictShare FROM
        Accomplices
        GROUP BY City
        HAVING (City = 'Chicago')
    ) AS CHICAGO;
