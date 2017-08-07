DB:=swen304_p1

all: report.pdf

%.pdf: %.md
	pandoc $< -o $@

conn:
	psql "$(DB)"

# SQL commands

reset:
	psql -c "DROP DATABASE $(DB)"
	psql -c "CREATE DATABASE $(DB)"

schema:
	psql "$(DB)" -f sql/up.sql

loaddata:
	psql "$(DB)" -c '\copy Banks from data/banks_17.data'
	psql "$(DB)" -c '\copy Robberies from data/robberies_17.data'
	psql "$(DB)" -c '\copy Plans from data/plans_17.data'
	psql "$(DB)" -c '\copy Robbers(Nickname, Age, NoYears) from data/robbers_17.data'
