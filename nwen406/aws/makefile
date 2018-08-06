ENV_NAME:=pyenv
AWS_ACCESS_KEY_ID?=
AWS_SECRET_ACCESS_KEY?=
AWS_SESSION_TOKEN?=

all: report.pdf

%.pdf: %.md
	pandoc -V geometry:margin=1in $< -o $@

clean:
	rm -f report.pdf

$(ENV_NAME):
	virtualenv $@

requirements: $(ENV_NAME) requirements.txt
	source ./$(ENV_NAME)/bin/activate ; \
	pip install -r requirements.txt ; \

list: requirements
	./$(ENV_NAME)/bin/aws lambda list-functions
