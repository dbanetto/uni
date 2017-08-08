DB:=swen304_p1

all: report.pdf

%.pdf: %.md
	pandoc $< -o $@

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
