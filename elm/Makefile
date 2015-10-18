
.PHONY: test

ELM_COMPILE=elm-make --yes --warn

ELM_IO = elm-stuff/packages/maxsnew/IO/1.0.1/elm-io.sh
TESTS=$(wildcard tests/*.elm)
TEST_JS=$(patsubst %.elm,%.elm.js,$(TESTS))

TARGETS=$(wildcard *.elm)
TARGETS_JS=$(patsubst %.elm,%.js,$(TARGETS))

all: compile

compile: $(TARGETS_JS)

%.js: %.elm
	@echo 'Compile $(ELM_COMPILE)  $<'
	$(ELM_COMPILE) $< --output $@


test: $(TESTS)
	elm-make --yes $< --output tests/raw.test.js
	$(ELM_IO) tests/raw.test.js $<.js
	node $<.js

clean-test:
	-@rm $(TEST_JS) test/raw.test.js

clean: clean-test
	-@rm $(TARGETS_JS)
