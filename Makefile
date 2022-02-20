ifndef VERBOSE
.SILENT:
endif
LD=
SRCDIR=src/
TESTDIR=test/
DIFFDIR=test-diff/
SRC=$(wildcard $(SRCDIR)*.hs)
PROJ=flp21-fun
LOGIN=xsovam00
TEST_INPUTS:=$(wildcard $(TESTDIR)*.in)

.PHONY:$(PROJ)
$(PROJ):
	ghc $(SRC) $(LD) -o $(PROJ)

run: $(PROJ)
	./$(PROJ) -2 < test/test00-2.in

clean:
	rm -rf $(PROJ) $(SRCDIR)*.hi $(SRCDIR)*.o $(DIFFDIR) *.zip

test:$(DIFFDIR) $(TEST_INPUTS)

$(TEST_INPUTS): $(TESTDIR)%.in: $(PROJ)
	if ./$< $(shell echo -n "$*" | tail -c 2) $@ | diff $(TESTDIR)$*.out - > $(DIFFDIR)$*.diff; then \
		echo "$* OK"; \
	else \
		echo "$* FAIL"; \
	fi

$(DIFFDIR):
	mkdir -p $(DIFFDIR)

pack: clean
	zip -r flp-fun-$(LOGIN).zip ./src ./doc ./test ./Makefile