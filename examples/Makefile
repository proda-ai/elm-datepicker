all: simple-example bootstrap-example range-example simple-nightwatch-example

.SHELL = /bin/bash
SHELL = /bin/bash

simple-example:
	cd .. && elm-make examples/simple/Simple.elm --output=examples/simple/simple.js

bootstrap-example:
	cd .. && elm-make examples/bootstrap/Bootstrap.elm --output=examples/bootstrap/bootstrap.js

range-example:
	cd .. && elm-make examples/range/Range.elm --output=examples/range/range.js

simple-nightwatch-example:
	cd .. && elm-make examples/simple-nightwatch/SimpleNightwatch.elm --output=examples/simple-nightwatch/simple-nightwatch.js
