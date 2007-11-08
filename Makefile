all:	PTS.hs
	ghc --make -c -Wall PTS.hs

.PHONY:	clean
clean:
	rm -rf *.o *.hi *~
