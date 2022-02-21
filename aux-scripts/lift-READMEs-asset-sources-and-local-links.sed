#!/bin/sed -f

### Lift/unlift links of Haskell modules in project's `README.md`:

s!\(\[[^]]*\]\)(\(.*\)\.hs\>!\1(src/\2.hs!;


### Lift/unlift image sources in project's `README.md`:

s~\(!\[[^]]*\]\)(doc/\(.*\.\)~\1(doc/assets/\2~;
