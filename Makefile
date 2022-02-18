LD=
SRC=flp21-fun.hs
PROJ=flp21-fun
LOGIN=xsovam00
.PHONY:$(PROJ)
$(PROJ):
	ghc $(SRC) $(LD) -o $(PROJ)

run: $(PROJ)
	./$(PROJ) -2 bkg-a2.in
	#./$(PROJ) -1 tin-a1.in
	#./$(PROJ) -2 < tin-a2.in

clean:
	rm $(PROJ) *.hi

pack:
	zip -r $(LOGIN).zip ./