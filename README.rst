.. role:: path(code)

This repo contains the Golog (swi-prolog version) implementation of my *situation calculus* formalisation of the *flow rule* language. It aligns with my *Logicalisation* file (to be renamed to *Formalisation*).

Different versions
-------------------

This repo uses *tags* for versioning. The relevant tags are named as `ver1`, `ver2`, etc.

The `master` branch contains the basic (and most reliable) implementation.

The `with-r` branch uses `rdel` to try to simplify the query.


Files explained
---------------

The :path:`golog_swi.pl` file is the Golog interpreter.

The :path:`flow_rule.pl` file is my implementation. It contains the directive to load :path:`golog_swi.pl`.

The sample knowledge base and queries are provided in the end of :path:`flow_rule.pl`.

Usage
-----

1. Load the file:

:code:`swipl flow_rule.pl`

2. Do your query (e.g. from the sample queries in :path:`flow_rule.pl`)

