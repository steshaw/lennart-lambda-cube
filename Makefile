.PHONY:	all
all:	cube

cube:	Main.hs Cube.hs
	ghc --make -Wall Main.hs -o cube

CUBES=	misc.cube bool.cube pair.cube maybe.cube either.cube nat.cube list.cube unit.cube void.cube exists.cube

.PHONY:	test
test:	cube $(CUBES)
	./cube - $(CUBES) test.cube

.PHONY:	clean
clean:
	rm -rf *.o *.hi *~ cube
