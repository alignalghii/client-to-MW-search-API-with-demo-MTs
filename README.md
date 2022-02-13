# Monad transformers demonstrated with a search API client

Monad transformers — an important field of how to design architecture for complex Haskell projects — can present a steep learning curve.
This little project tries to provide a small motivating example.

The non-pedagogical, naked technical goal of the project is to provide an API client softwer to the [API:Search](https://www.mediawiki.org/wiki/API:Search) service of MediaWiki.

Wikipedia has many interesting articles and other useful resources (e.g. images, videos) in the most various topics.
The user can navigate simply reading the articles and jumping throught their links from article to article, or use the category labels, or read portal-like articles summarizing many fields of a broad topic. But besides all these link-based tools, there is also a search feature, mostly used by readers of Wikipedia. This feature is available not only for direct human use: Wikipedia also provides an API, capable of finding a listing various documents based on the searchphrase provivd by the user.

The [API:Search](https://www.mediawiki.org/wiki/API:Search) site of MediaWiki describes the use of this API. At the URL address of the service, the searchphrase can be provided with the `&rsearch=` particle of a `GET` request. To experiment with the service interactively, here is the sandbox site for that: [API sandbox](https://en.wikipedia.org/wiki/Special:ApiSandbox#action=query&list=search&srsearch=Haskell&utf8=&format=json), exemplified here with searchphrase „*Haskell*”.

The search results come in a paginated way: the results contain

- a limited number of the found items themselves,
- plus an optional „continuation token” — `sroffset` —, it is a natural number.

The user can provide this continuation token in his/her next search alongside with the searchprase, then the server will provide a continuation of the search.


##Usage

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

## Examples

An examaple for the most common case: there are so many result items that pagination is needed:

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

An example where pagination is not needed, because there are fewer than 10 result title items:

```
me@my-computer:~/haskell/crawler$ ./crawler --search=Vackor
Service: first-page of search result for searchphase `Vackor'
 - Vackor az első bében
 - Europa postage stamp
 - Proton Theatre
No more search results
me@my-computer:~/haskell/crawler$
```

An example where pagination is not needed, because there are no result title items at all:

```
me@my-computer:~/haskell/crawler$ ./crawler --search=tughneghaq
Service: first-page of search result for searchphase `tughneghaq'
No more search results
me@my-computer:~/haskell/crawler$
```

## Tests

### Unit tests

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

### Integrated tests

### Laziness experimentation