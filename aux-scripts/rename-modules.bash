#!/bin/bash

for modu in `git status | sed -n '/renamed:/s/.*renamed:\s*\(\w*\).*/\1/p'`;
	do sed -i "/import\|module $modu/s/$modu/Service.$modu/" `grep -l "\(import\|module\) $modu" -nr . --include='*.hs'`;
done;
