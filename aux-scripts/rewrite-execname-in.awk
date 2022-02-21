#!/usr/bin/gawk -f

/# [Ee]xec:/ {e=NR+1;}

NR==e {sub(/.*/, execname);}

{print;}
