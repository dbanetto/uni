#!/bin/bash -x

CONTEXT=$(realpath `dirname $0`)

NAME=$2
REPO=$1
MONO=$CONTEXT/mono
FOLDER=$CONTEXT/$REPO
DIR=$CONTEXT/$REPO/$NAME

cd $FOLDER

ls

mkdir $NAME

mv -f * $NAME
mv -f .gitignore $NAME

git add .
git commit -m 'restructure for mono-repo'

cd $MONO

git remote add $NAME $FOLDER
git fetch $NAME
git merge --allow-unrelated-histories --no-edit $NAME/master
git remote rm $NAME
