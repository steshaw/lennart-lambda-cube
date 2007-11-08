GHC=	ghc

.PHONY:	all
all:	cube

cube:	Cube.hs CubeExpr.hs
	$(GHC) --make -Wall Cube.hs -o cube

CUBES=	misc.cube bool.cube pair.cube maybe.cube either.cube nat.cube list.cube listmisc.cube unit.cube void.cube exists.cube

.PHONY:	test
test:	cube $(CUBES)
	./cube - $(CUBES) test.cube > test.out
	cmp test.ok test.out && echo Test OK

.PHONY:	clean
clean:
	rm -rf *.o *.hi *~ cube setup dist test.out
