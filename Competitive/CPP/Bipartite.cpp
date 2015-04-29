#include <algorithm> 
#include <iostream>
#include <stdio.h>
#include <string.h>
#include <vector>

//This solution is for UVA Judge problem 10004.
//This is determining if a graph is bipartite.
//Accepted 2014/04/23.
int bipartite_set(std::vector<int> sets[2], int key);

void printSet(std::vector<int> set, std::string name);

void printSets(std::vector<std::vector<int> > sets);

void printAdjMat(char* adjMat,int width);

int main()
{
	while(1){
		int bipartite = 1;
		std::string input;
    std::cout << "Give me a number of nodes, or 0 to quit." << std::endl;
		getline(std::cin, input);
		int numnodes = atoi(input.c_str());
		if(!numnodes)
			break;
    std::cout << "How many edges in this graph?" << std::endl;
		getline(std::cin, input);
		int numedges = atoi(input.c_str());
		int index;
		//Create the adjacency matrix.
		char *adjMat = (char*)malloc(sizeof(char) * numnodes * numnodes);
		for(index = 0; index < numnodes * numnodes; index++){
			adjMat[index] = 0;
		}
    std::cout << "Enter edges in form \"from to\"." << std::endl;
		for(index = 0; index < numedges; index++){
			int first;
      int secnd;
		  getline(std::cin, input);
			sscanf(input.c_str(), "%d %d", &first, &secnd);
			adjMat[first * numnodes + secnd] = 1;
			adjMat[secnd * numnodes + first] = 1;
		}
		//BFS
		std::vector<int> redset, blueset;
		std::vector<int> sets[2];
		sets[0].push_back(0);
		std::vector<int> queue(1, 0);//The stuff we still need to label.
		//While queue is not empty
		while (bipartite && queue.size())
    {
			//Pop its head off
			int node = queue.front();
			queue.erase(queue.begin());
			//Find what set its in
			int set = bipartite_set(sets,node);
			//For each other node
			for (index = 0; index < numnodes; index++)
      {
				//If they're neighbors.
				if (node != index && adjMat[node * numnodes + index])
        {
					int neighborset = bipartite_set(sets, index);
					//If they are in the same set, bipartite=0 and break.
					if (neighborset == set)
          {
						bipartite = 0;
						break;
					}
					//If they aren't in a set, add them to the opposite and the DFS queue.
					if (neighborset == -1)
          {
						sets[!set].push_back(index);
						queue.push_back(index);
					}
				}				
			}		
		}
		//Print Results
		if (bipartite)
    {
			printf("BICOLORABLE.\n");
		}
		else
			printf("NOT BICOLORABLE.\n");
		queue.clear();
		free(adjMat);
	}
}
void printSet(std::vector<int> set, std::string name) {
  printf("%s set:", name.c_str());
	for (int mem : set)
  {
		printf("%d ", mem);
	}
	printf("\n");

}

void printSets(std::vector<std::vector<int> >sets){
	printSet(sets[0], "Red");
	printSet(sets[1], "Blue");
  /*printf("Red set:");
	for (int mem : sets[0])
  {
		printf("%d ", mem);
	}
	printf("\n");
	printf("Blue set:");
	for (int mem : sets[1])
  {
		printf("%d ", mem);
	}
	printf("\n");*/
}

int bipartite_set(std::vector<int> sets[2], int key){
	//return 0 if key is in sets[0]
	//return 1 if key is in sets[1]
	//return -1 otherwise.
	int set;
	for (set = 0; set < 2; set++)
  {
		std::vector<int> vec = sets[set];
		if (std::find(vec.begin(), vec.end(), key) != vec.end())
    {
			return set;
		}
	}
	return -1;
}

void printAdjMat(char *adjMat, int width){
	printf("Printing Adjacency Matrix\n");
	for(int x = 0; x < width; x++)
  {
		for(int y = 0; y < width; y++)
    {
			printf("%d ", adjMat[y * width + x]);
		}
		printf("\n");
	}
	printf("###\n");
}
