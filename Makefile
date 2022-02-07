PROGNAME=client-to-MW-search-API-with-demo-MTs

executable:
	ghc Main -o $(PROGNAME);
	gawk -i inplace '/# [Ee]xec:/ {e=NR+1} NR==e {sub(/.*/, "$(PROGNAME)");} {print}' .gitignore; # Comment this out if `gawk` is old/missing!
clean:
	find . -type f -name '*.hi' -delete;
	find . -type f -name '*.o' -delete;
	if test -f Main; then rm Main; fi;
	if test -f $(PROGNAME); then rm $(PROGNAME); fi;
