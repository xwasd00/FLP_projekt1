LD=
SRC=flp21-fun.hs
PROJ=flp21-fun
LOGIN=xsovam00
.PHONY:$(PROJ)
$(PROJ):
	ghc $(SRC) $(LD) -o $(PROJ)

run: $(PROJ)
	#./$(PROJ) -2 bkg.in
	./$(PROJ) -1 tin.in

clean:
	rm $(PROJ) *.hi

pack:
	zip -r $(LOGIN).zip ./