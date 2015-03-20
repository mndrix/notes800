# Synopsis

```prolog
:- use_module(library(notes800), []).
?- notes800:caller_id("1-877-528-5852",Caller,Type).
Caller = "ERG",
Type = debt_collector.
```

# Description

[800notes](http://800notes.com/) is one of the largest communities for reporting and discussing  telephone numbers (telemarketing, prank calls, surveys, etc).  This pack provides a convenient way to query that site for details about a specific number.

# Installation

Using SWI-Prolog 7.1 or later:

    ?- pack_install(notes800).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/notes800
