EXECNAME=client-to-MW-search-API-with-demo-MTs

executable:
	ghc Main -o $(EXECNAME);
	../aux-scripts/rewrite-execname-in.bash $(EXECNAME) ../.gitignore;
clean:
	find . -type f -name '*.hi' -delete;
	find . -type f -name '*.o' -delete;
	if test -f Main; then rm Main; fi;
	if test -f $(EXECNAME); then rm $(EXECNAME); fi;
