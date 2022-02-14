# Monad transformers demonstrated with a search API client

## Table of contents

- [Table of contents](#table-of-contents)
- [Introduction](#introduction)
- [Usage](#usage)
    - [Examples](#examples)
- [Architecture](#architecture)
    - [Theoretical stack of monad transformers](#theoretical-stack-of-monad-transformers)
    - [Factual stack](#factual-stack)
- [Meta-features: automatic tests, experiments](#meta-features-automatic-tests-experiments)
    - [Unit tests](#unit-tests)
    - [Integration tests](#integration-tests)
    - [Laziness experimentation](#laziness-experimentation)

## Introduction

Monad transformers — an important field of how to design architecture for complex Haskell projects — can present a steep learning curve.
This little project tries to provide a small motivating example.

The non-pedagogical, naked technical goal of the project is to provide an API client softwer to the [API:Search](https://www.mediawiki.org/wiki/API:Search) service of MediaWiki.

Wikipedia has many interesting articles and other useful resources (e.g. images, videos) in the most various topics.
The user can navigate simply reading the articles and jumping throught their links from article to article, or use the category labels, or read portal-like articles summarizing many fields of a broad topic. But besides all these link-based tools, there is also a search feature, mostly used by readers of Wikipedia. This feature is available not only for direct human use: Wikipedia also provides an API, capable of finding a listing various documents based on the searchphrase provivd by the user.

The [API:Search](https://www.mediawiki.org/wiki/API:Search) site of MediaWiki describes the use of this API. At the URL address of the service, the searchphrase can be provided with the `&rsearch=` particle of a `GET` request. To experiment with the service interactively, here is the sandbox site for that: [API sandbox](https://en.wikipedia.org/wiki/Special:ApiSandbox#action=query&list=search&srsearch=Haskell&utf8=&format=json), exemplified here with searchphrase „*Haskell*”.

The search results come in a paginated way: the results contain

- a limited number of the found items themselves (ten ones),
- plus an optional „continuation token” — `sroffset` —, it is a natural number.

The user can provide this continuation token in his/her next search alongside with the searchprase, in order  to instruct the server to provide a continuation of the search with the next ten items.


## Usage

The program is a command-line tool. `Makefile` names the executable file `client-to-MW-search-API-with-demo-MTs`. A help info can be seen when calling the program name alone, or with option `--help`:

```
me@my-computer:~/haskell/crawler$ ./client-to-MW-search-API-with-demo-MTs --help
Usage: client-to-MW-search-API-with-demo-MTs <OPTION>
where <OPTION> can be *one* of the following options:
  -S <EXPR>  --search=<EXPR>, --service=<EXPR>   # Consult with Wikipedia's search service: search for <EXPR>
  -t         --test                              # Run unit tests
  -h, -?     --help                              # Info about the command-line interface (flags, options)

Abbreviations:
    `client-to-MW-search-API-with-demo-MTs` alone is interpreted as `client-to-MW-search-API-with-demo-MTs --help`
    `client-to-MW-search-API-with-demo-MTs <EXPR>` is interpreted as `client-to-MW-search-API-with-demo-MTs --search <EXPR>`
me@my-computer:~/haskell/crawler$
```

### Examples

The most  useful command-line options are `--search=`, and its improved version, `--paginate=`. Both these options require a searchphrase argument. For example, if the user wants to see a few Wikipedia article titles for searchphrase „Haskell”, he/hse can issue the command
```
./client-to-MW-search-API-with-demo-MTs --search=Haskell
```
But if she/he wants to get more search results than the first ten items (i.e. he/she wants to paginate among the result interactively, or evenmore wants to see all results), then he/she must issue
```
./client-to-MW-search-API-with-demo-MTs --paginate=Haskell
```

An example for the most common case: there are so many result items that pagination is needed:

```
me@my-computer:~/haskell/crawler$ ./client-to-MW-search-API-with-demo-MTs --search=Haskell
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

As the example shows, `--search=` is a rather low-level option, the need for pagination is signalled to the user, but the command does not help in that any further. The behavior of the more developed `--paginate=` option is more user-friendly in pagination:

```
me@my-computer:~/haskell/crawler$ ./client-to-MW-search-API-with-demo-MTs --paginate=Haskell
Service: paginated search result for searchphase `Haskell'
 - Haskell
 - Haskell (programming language)
 - Will Haskell
 - James Haskell
 - Gordon Haskell
 - Colleen Haskell
 - Eddie Haskell
 - Haskell (surname)
 - Miriam Haskell
 - David Haskell
Type `y' or `yes' to continue search, anything else (or a single ENTER) to cancel! (Continuation token: 10)
yes
 - Miriam Haskell
 - Haskell Wexler
 - Peter Haskell
 - Haskell Platform
 - Haskell (company)
 - Molly Haskell
 - Glasgow Haskell Compiler
 - Haskell Curry
 - Neil Haskell
 - Haskell Indian Nations University
Type `y' or `yes' to continue search, anything else (or a single ENTER) to cancel! (Continuation token: 20)
yes
 - Haskell, Oklahoma
 - Haskell Garrett
 - Jamie Haskell
 - Liquid Haskell
 - Floyd Haskell
 - Todd Philip Haskell
 - Haskell features
 - Edward Haskell
 - Haskell Stakes
 - Haskell, Texas
Type `y' or `yes' to continue search, anything else (or a single ENTER) to cancel! (Continuation token: 30)
no
me@my-computer:~/haskell/crawler$
```

An example where pagination is not needed, because there are fewer than 10 result title items:

```
me@my-computer:~/haskell/crawler$ ./client-to-MW-search-API-with-demo-MTs --search=Vackor
Service: first-page of search result for searchphase `Vackor'
 - Vackor az első bében
 - Europa postage stamp
 - Proton Theatre
No more search results
me@my-computer:~/haskell/crawler$
```

An example where pagination is not needed, because there are no result title items at all:

```
me@my-computer:~/haskell/crawler$ ./client-to-MW-search-API-with-demo-MTs --search=tughneghaq
Service: first-page of search result for searchphase `tughneghaq'
No more search results
me@my-computer:~/haskell/crawler$
```

## Architecture

### Theoretical stack of monad transformers

The overall architercture can be presented in a concide way with the following types:

```haskell
type PaginationToken   = Int
type PaginationControl = Maybe PaginationToken -- Both the start and stop states are represented by `Nothing`

data ErrorType  = FailedConnection | UnexpectedResponseFormat
type ErrorMonad = Either ErrorType

type DocumentTitle = String

type Architecture = StateT PaginationControl (ErrorMonad IO) [DocumentTitle]
```

### Factual stack

Practicaly, in the current stage of the development, a lot of more low-level solutions are used. Contentually, they express the same, as the above scheme, but they have not yet been brought to this conceptually clear format.

For example, often a transition function is used instead of an explicit state monad. In all these cases, an implicit state monad is hiding behind the use and the context of the transition function.

The project intends to be somewhat didactical and historical here (at the cost of losing conciseness). The following modules present a kind of wandering from the most naive styles of state respresentations toward more and more explicit and standard formalisms:

- Control
    - [`Transition`](Control/Transition.hs)
    - [`Pagination`](Control/Pagination.hs)
- `PaginationStatemachines`
    - `Effectless`
        - [`PaginationConceptSeries`](PaginationStateMachines/Effectless/PaginationConceptSeries.hs)
        - [`PaginationConceptSeriesSpec`](PaginationStateMachines/Effectless/PaginationConceptSeriesSpec.hs)
    - `Effectful`
        - [`PaginationConceptSeries`](PaginationStateMachines/Effectful/PaginationConceptSeries.hs)
        - [`PaginationConceptSeriesSpec`](PaginationStateMachines/Effectful/PaginationConceptSeriesSpec.hs)

## Meta-features: automatic tests, experiments

### Unit tests


Unit tests can be invoked by the user simply as part of the command-line options:

```
me@my-computer:~/haskell/crawler$ ./client-to-MW-search-API-with-demo-MTs --unit-test

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

Finished in 0.0063 seconds
7 examples, 0 failures
me@my-computer:~/haskell/crawler$
```

Technically, in the surcecode, unit tests are „pure” tests on a state machine without the „unpure” `IO` monad:

- `PaginationStatemachines`
    - `Effectless`
        - [`PaginationConceptSeries`](PaginationStateMachines/Effectless/PaginationConceptSeries.hs)
        - [`PaginationConceptSeriesSpec`](PaginationStateMachines/Effectless/PaginationConceptSeriesSpec.hs)


Even those monad transformers are tested without `IO`, which work together in the edge application with `IO`. In their corresponding unit test the `IO` part of their transfomer stack is replaced with the „harmless”, „purer” `ErrorMonad`:

- `Effectful`
    - [`PaginationConceptSeries`](PaginationStateMachines/Effectful/PaginationConceptSeries.hs)
    - [`PaginationConceptSeriesSpec`](PaginationStateMachines/Effectful/PaginationConceptSeriesSpec.hs)


### Integration tests

```
me@my-computer:~/haskell/crawler$ ./client-to-MW-search-API-with-demo-MTs --intgr-test
Integration tests:
  ✓  Invalid JSON format response from server is handler by this client correctly: True
me@my-computer:~/haskell/crawler$
```

### Laziness experimentation

```
Laziness of Identity, Maybe and IO monads on top of lazy State monad transformer:
=================================================================================
 ✓  laziness of the lazy State monad: [0,1,2,3,4,5,6,7,8,9,10,11]
 ✓  laziness of Identity monad on top of the lazy State monad transformer (StateT s Identity a): Identity [0,1,2,3,4,5,6,7,8,9,10,11]
 ✕  laziness of Maybe monad on top of the lazy State monad transformer (StateT s Maybe a): <<!!-INFINITE-LOOP-!!>>
 ✕  laziness of IO monad on top of the lazy State monad transformer (StateT s IO a): <<!!-INFINITE-LOOP-!!>>
```

### Laziness experimentation