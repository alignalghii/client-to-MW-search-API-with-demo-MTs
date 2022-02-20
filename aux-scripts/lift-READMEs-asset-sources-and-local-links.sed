#!/bin/sed -f

#################################################
# Lift/unlift links of Haskell modules in README:

s!\(\[[^]]*\]\)(\(.*\)\.hs\>!\1(src/\2.hs!;

######################################
# Lift/unlift image sources in README:

s~\(!\[[^]]*\]\)(doc/\(.*\.\)~\1(doc/assets/\2~;
