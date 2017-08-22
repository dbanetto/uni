DB:=barnetdavi_swen304_a3

all: report.pdf

%.pdf: %.md
	pandoc -V geometry:margin=1in $< -o $@

reset:
	psql -c "DROP DATABASE IF EXISTS $(DB)"
	psql -c "CREATE DATABASE $(DB)"
	psql -c "REVOKE CONNECT ON DATABASE $(DB) FROM PUBLIC"

data: reset
	psql "$(DB)" -f sql/swen304_a3_q1.sql
	psql "$(DB)" -f sql/swen304_a3_q2.sql

test: data
	psql "$(DB)" -f test/swen304_a3_q1_test.sql
	psql "$(DB)" -f test/swen304_a3_q2_test.sql
