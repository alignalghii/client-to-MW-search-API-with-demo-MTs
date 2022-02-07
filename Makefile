PROGNAME=search-client-MW-demo-MTs

executable:
	ghc Main -o $(PROGNAME);
	gawk -i inplace '/# [Ee]xec:/ {e=NR+1} NR==e {sub(/.*/, "$(PROGNAME)");} {print}' .gitignore; # You can outcomment this if it causes trouble
clean:
	find . -type f -name '*.hi' -delete;
	find . -type f -name '*.o' -delete;
	if test -f Main; then rm Main; fi;
	if test -f $(PROGNAME); then rm $(PROGNAME); fi;
