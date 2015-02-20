#!/bin/bash

if [ -e "$1" ]
then
java -cp "lib/*" -Dgov.nih.cit.soccer.wordnet.dir="/local/content/soccer/dict" -Dgov.nih.cit.soccer.output.dir="/usr/local/tomee/webapps/soccer/files" gov.nih.cit.soccer.Soccer $1
else
echo "$1 does not exist!"
fi

