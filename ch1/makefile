all : out quux.txt
	./out < quux.txt

out: WC.hs
	ghc -o out $<

.PHONY: clean
clean:
	rm out
	rm *.hi
	rm *.o