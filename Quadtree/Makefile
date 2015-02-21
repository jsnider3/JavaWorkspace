all: Quadtree_Main
	@true

Quadtree_Main: Quadtree_Main.cc City.o Quadtree.o
	g++ -g Quadtree_Main.cc City.o Quadtree.o -o Quadtree_Main

City.o: City.cc City.hh
	g++ -g -c City.cc

Quadtree.o: Quadtree.cc Quadtree.hh
	g++ -g -c Quadtree.cc

clean:
	@rm *.o Quadtree_Main | true 
