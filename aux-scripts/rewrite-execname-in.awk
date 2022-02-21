#!/usr/bin/gawk -f

/# [Ee]xec:/ {
	lineOfExecFile = NR + 1;
}

NR == lineOfExecFile {
	sub(/.*/, execName);
}

{
	print;
}

####################################################################
# In the .gitignore file of the overall project,
# also the executable is listed as to be ignored.
# Convention: the name of the executable is comes after a header line „# Exec:”.
#
# This AWK script finds the „# Exec:” header, and regards the next line as the name of the executable.
# It can rewrite this name to a custom name (provoded in the `execName` variable coming from command-line.
#
# This AWK script in not used staandalone: it is called by a same-named bash script.
####################################################################
