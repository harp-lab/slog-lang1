#!/bin/bash
REQUIRED_VERSION=1.8
if [ $# -eq 2 ]
then
	#set JAVA_HOME to root of any java 1.8 installation or set JAVA_HOME variable in profile
	#e.g. JAVA_HOME=/usr/lib/jvm/jdk1.8.0
	JAVA_EXE=$JAVA_HOME/bin/java
	version=$("$JAVA_EXE" -version 2>&1 | awk -F '"' '/version/ {print $2}')
	#echo version "$version"
	if [[ "$version" < $REQUIRED_VERSION ]]; then
        	echo Java $REQUIRED_VERSION or newer is required to run DeALS.
		exit
	fi

	exec $JAVA_EXE -Xmx4G -classpath $1 edu.ucla.cs.wis.bigdatalog.system.FileRunner $2 
else
echo 'Please provide file to run. "./runfile.sh [deal.jar] [filename.deal]"'
fi

