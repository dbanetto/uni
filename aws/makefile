ENV_NAME:=pyenv
AWS_ACCESS_KEY_ID?=
AWS_SECRET_ACCESS_KEY?=
AWS_SESSION_TOKEN?=

all: requirements

$(ENV_NAME):
	virtualenv $@

requirements: $(ENV_NAME) requirements.txt
	source ./$(ENV_NAME)/bin/activate ; \
	pip install -r requirements.txt ; \

list: requirements
	./$(ENV_NAME)/bin/aws lambda list-functions
