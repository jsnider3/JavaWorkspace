all: Quadtree_Main
	@true

Quadtree_Main: Quadtree_Main.cc City.o Quadtree.o
	g++ -std=c++11 -g -Wall Quadtree_Main.cc City.o Quadtree.o -o Quadtree_Main

City.o: City.cc City.hh
	g++ -std=c++11 -g -Wall -c City.cc

Quadtree.o: Quadtree.cc Quadtree.hh
	g++ -std=c++11 -g -Wall -c Quadtree.cc

clean:
	@echo "clean"
	@rm *.o Quadtree_Main 2>/dev/null | true 
