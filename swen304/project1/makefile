DB:=barnetdavi_swen304_p1

all: report.pdf

%.pdf: %.md
	pandoc -V geometry:margin=1in $< -o $@ 

clean:
	rm report.pdf

conn:
	psql "$(DB)"

# SQL commands

reset:
	psql -c "DROP DATABASE IF EXISTS $(DB)"
	psql -c "CREATE DATABASE $(DB)"

schema:
	psql "$(DB)" -f sql/schema.sql

loaddata:
	psql "$(DB)" -f sql/load.sql

insert:
	psql "$(DB)" -f sql/insert.sql

query:
	find sql/ -type f -name 'Query4_Task*.sql' -exec psql "$(DB)" -f {} \;

complex:
	find sql/ -type f -name 'Query5_Task*.sql' -exec psql "$(DB)" -f {} \;

