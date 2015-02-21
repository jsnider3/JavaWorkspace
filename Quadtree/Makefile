all: Quadtree_Main
	@true

Quadtree_Main: Quadtree_Main.cc City.o Quadtree.o
	g++ -g -Wall Quadtree_Main.cc City.o Quadtree.o -o Quadtree_Main

City.o: City.cc City.hh
	g++ -g -Wall -c City.cc

Quadtree.o: Quadtree.cc Quadtree.hh
	g++ -g -Wall -c Quadtree.cc

clean:
	@echo "clean"
	@rm *.o Quadtree_Main 2>/dev/null | true 
