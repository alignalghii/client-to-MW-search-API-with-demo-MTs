The [API:Search](https://www.mediawiki.org/wiki/API:Search) site of MediaWiki.

Usage:

```
me@my-computer:~/haskell/crawler$ ./crawler --help
Usage: crawler <OPTION>
where <OPTION> can be *one* of the following options:
  -S <EXPR>  --search=<EXPR>, --service=<EXPR>   # Consult with Wikipedia's search service: search for <EXPR>
  -t         --test                              # Run unit tests
  -h, -?     --help                              # Info about the command-line interface (flags, options)

Abbreviations:
    `crawler` alone is interpreted as `crawler --help`
    `crawler <EXPR>` is interpreted as `crawler --search <EXPR>`
me@my-computer:~/haskell/crawler$
```

Examples:

```
me@my-computer:~/haskell/crawler$ ./crawler --search=Haskell
Service: first-page of search result for searchphase `Haskell'
 - Haskell
 - Haskell (programming language)
 - Will Haskell
 - Colleen Haskell
 - James Haskell
 - Eddie Haskell
 - Gordon Haskell
 - Haskell Curry
 - Floyd Haskell
 - Peter Haskell
There are more results, repeat seach with &sroffset=10
me@my-computer:~/haskell/crawler$
```

```
me@my-computer:~/haskell/crawler$ ./crawler --search=Vackor
Service: first-page of search result for searchphase `Vackor'
 - Vackor az első bében
 - Europa postage stamp
 - Proton Theatre
No more search results
me@my-computer:~/haskell/crawler$
```

```
me@my-computer:~/haskell/crawler$ ./crawler --search=tughneghaq
Service: first-page of search result for searchphase `tughneghaq'
No more search results
me@my-computer:~/haskell/crawler$
```

Unit tests:

```
Infinite effectless pagination
  Both infinite state-jumps and finite state-jumps can have an infinite runtime
    God's infinite book with infinite state-jumps
    A ticking clock with finite state-jumps but infinite runtime
Limitable effectless pagination
  There are only four seasons, the sample is limited
    simple-function non-DRY solution
    simple-function but more economical (DRY) solution
    State-monad solution
Effectful pagination
  Biliography server
    Without state transformer
    With state transformer

Finished in 0.0008 seconds
7 examples, 0 failures
me@my-computer:~/haskell/crawler$ ./crawler --help
```

