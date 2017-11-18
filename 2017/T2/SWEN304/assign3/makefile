DB:=barnetdavi_swen304_a3

all: report.pdf

%.pdf: %.md
	pandoc -V geometry:margin=1in $< -o $@

clean:
	psql -c "DROP DATABASE IF EXISTS $(DB)"

setup:
	psql -c "CREATE DATABASE $(DB)"
	psql -c "REVOKE CONNECT ON DATABASE $(DB) FROM PUBLIC"

reset: clean setup

q1: reset
	psql "$(DB)" -f sql/swen304_a3_q1.sql
	psql "$(DB)" -f sql/q1_trigger.sql
	psql "$(DB)" -f test/swen304_a3_q1_test.sql

q2: reset
	psql "$(DB)" -f sql/swen304_a3_q2.sql
	psql "$(DB)" -f sql/q2_udf.sql
	psql "$(DB)" -f test/swen304_a3_q2_test.sql

conn:
	psql "$(DB)"
