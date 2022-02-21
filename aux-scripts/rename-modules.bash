#!/bin/bash

for module in `git status | sed -n '/renamed:/s/.*renamed:\s*\(\w*\).*/\1/p'`;
	do sed -i "/import\|module $module/s/$module/Service.$module/" `grep -l "\(import\|module\) $module" -nr . --include='*.hs'`;
done;
