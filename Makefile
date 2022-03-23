ifndef VERBOSE
.SILENT:
endif
SRCDIR=src/
TESTDIR=test/
DIFFDIR=test-diff/
SRC=$(wildcard $(SRCDIR)*.hs)
PROJ=flp21-fun
LOGIN=xsovam00
ZIP=flp-fun-$(LOGIN).zip
TEST_INPUTS:=$(wildcard $(TESTDIR)*.in)

.PHONY:$(PROJ)
$(PROJ):
	ghc $(SRC) -Wall -o $(PROJ)

run: $(PROJ)
	./$(PROJ) -2 < test/test00-2.in

clean:
	rm -rf $(PROJ) $(SRCDIR)*.hi $(SRCDIR)*.o $(DIFFDIR) $(ZIP)

test:$(DIFFDIR) $(TEST_INPUTS)

$(TEST_INPUTS): $(TESTDIR)%.in: $(PROJ)
	if ./$< $(shell echo -n "$*" | tail -c 2) $@ > $(DIFFDIR)$*.out && diff -c $(TESTDIR)$*.out $(DIFFDIR)$*.out > $(DIFFDIR)$*.diff; then \
		echo "$* OK"; \
	else \
		echo "$* FAIL - see in directory $(DIFFDIR)"; \
	fi

$(DIFFDIR):
	mkdir -p $(DIFFDIR)

pack: clean
	zip -r $(ZIP) ./src ./doc ./test ./Makefile