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
