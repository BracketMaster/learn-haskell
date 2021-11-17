all : out quux.txt
	./out

out: BookStore.hs
	ghc -o out $<

.PHONY: clean
clean:
	rm out
	rm *.hi
	rm *.o