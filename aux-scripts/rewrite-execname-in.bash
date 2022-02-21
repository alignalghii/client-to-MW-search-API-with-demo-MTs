#!/bin/bash

here=`dirname "$0"`;
# it can differ from `pwd` if the script is called from elsewhere (just like now: called from `src/Makefile`)
# Credit to: https://stackoverflow.com/questions/59895/how-can-i-get-the-source-directory-of-a-bash-script-from-within-the-script-itsel

if test $# -ge 2;
	then
		execName="$1"; cfgFileName="$2"; shift 2;
		if which gawk > /dev/null && true; # Set to false if You do not want AWK to update Your `.gitignore` with the name of the executable
			then gawk -i inplace -v execName="$execName" -f $here/rewrite-execname-in.awk "$cfgFileName";
			else echo 'AWK missing or disabled, it cannot update Your `.gitignore` with the name of the executable';
		fi;
	else
		echo "Provide the name of the execfile and the name of the config/gitignorefile of Your project as command-line argunents!" >&2;
		exit 1;
fi;

####################################################################
# In the .gitignore file of the overall project,
# also the executable is listed as to be ignored.
# Convention: the name of the executable is comes after a header line „# Exec:”.
#
# This bash script finds the „# Exec:” header in .gitignore, and regards the next line as the name of the executable.
# It can rewrite this name to a custom name (provoded in the `execName` variable coming from command-line.
#
# This bash script is valled from Makefile at each build.
####################################################################
