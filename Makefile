executable:
	ghc Main -o crawler
clean:
	find . -type f -name '*.hi' -delete;
	find . -type f -name '*.o' -delete;
	if test -f Main; then rm Main; fi;
	if test -f crawler; then rm crawler; fi;
