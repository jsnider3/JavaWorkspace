#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "MST.h"


int main(){
  for(int count = 1; ; count++)
  {
		int input = 0;
    printf("Enter number of edges, <1 exits.\n");
		scanf("%d",&input);
		if(input < 1){
			exit(0);
		}
		AdjPoint listHead=getList(input);
		//printList(listHead);
		AdjPoint MSTHead = getMST(listHead);
		printMST(MSTHead, count);
		freeList(listHead);
		freeList(MSTHead);
	}
}

AdjPoint getList(int input){
	//Get the tree from user input.
	AdjPoint listHead = NULL;
	int counts = 0;
  printf("Give edges in the form of \"from to weight\"\n");
	while (counts < input)
  {
		AdjPoint newHead = (AdjPoint) malloc(sizeof(AdjRecord));
		newHead->next = listHead;
		listHead = newHead;
		scanf("%d %d %d", &(listHead->firstnode), &(listHead->secondnode),
      &(listHead->weight));
		counts++;
	}
	//Return the head of a linked list of edges.
	return listHead;
}

void printList(AdjPoint listHead){
	//Print the nodes in the list, by recursively walking the list.
	if(listHead != NULL){
		printf("%d -> %d %d\n", listHead->firstnode, listHead->secondnode,
      listHead->weight);
		printList(listHead->next);
	}
}

AdjPoint addEmptyAdjPointToFront(AdjPoint listHead){
	//Creates an empty node and adds it to the front of the list.
	AdjPoint newHead = (AdjPoint) malloc(sizeof(AdjRecord));
	newHead->next = listHead;
	return newHead;
}

AdjPoint getMST(AdjPoint listHead){
	//Since there's nothing in the minimal spanning tree, pick the edge with the lowest weight and add it to the MST.
	AdjPoint MSTHead = getNextEdge(listHead, NULL);
	while(1){
		AdjPoint temp = getNextEdge(listHead, MSTHead);//Get the next edge we should add.
		if(temp == NULL)
    {
      //If there's nothing more to add, return.
			return MSTHead;
		}
		else
    {
      //If we have more to add, add it to the front.
			temp->next = MSTHead;
			MSTHead = temp;
		}
	}
}


AdjPoint getNextEdge(AdjPoint listHead, AdjPoint MSTHead)
{
	//Search the linkedList of edges and return either a pointer
  // to an edge that should be added to MSTHead or NULL if 
  // the MST is complete.
	int minWeight = INT_MAX;
	AdjPoint min = NULL;
	while (listHead != NULL)
  {
		int bool = listHead->weight <= minWeight &&
               canBeAdded(listHead->firstnode, listHead->secondnode, MSTHead);
		if (bool)
    {
			minWeight = listHead->weight;
			min = listHead;
		}
		listHead = listHead->next;
	}
	//If min is null, go ahead and return that. 
  //Otherwise we need to make a copy so we don't disturb the tree.
	AdjPoint copy = AdjCopy(min);
	return copy;
}

AdjPoint AdjCopy(AdjPoint edge)
{
	//Returns a copy of edge with the same nodes and weight.
  AdjPoint copy = edge;
  if (edge != NULL)
  {
	  copy = (AdjPoint)malloc(sizeof(AdjRecord));
	  copy->firstnode = edge->firstnode;
	  copy->secondnode = edge->secondnode;
	  copy->weight = edge->weight;
	  copy->next = NULL;
  }
	return copy;
}

int canBeAdded(int firstnode, int secondnode, AdjPoint MSTHead)
{
	//True if one of them is in the list and the other isn't or 
  //if the MST is null and we're supposed to be initializing it.
	return MSTHead == NULL || 
        (contains(firstnode, MSTHead) && !contains(secondnode, MSTHead)) ||
        !contains(firstnode, MSTHead) && contains(secondnode, MSTHead);
}

int contains(int node, AdjPoint MSTHead)
{
	//Walk the minimal spanning tree's edge list. 
  //Return true if the node represented by the parameter node is 
  //connected to one of the edges in the MST.
	while (MSTHead != NULL)
  {
		if (MSTHead->firstnode == node || MSTHead->secondnode == node)
    {
			return 1;
		}
		MSTHead = MSTHead->next;
	}
	return 0;
}

void printMST(AdjPoint MSTHead, int MST){
	printf("Edges in MST %d.\n", MST);
	printList(MSTHead);
	printf("Weight = %d\n", sumWeights(MSTHead));
}

int sumWeights(AdjPoint MSTHead){
	//Get the total weight of the MST.
  int weight = 0;
  while (MSTHead != NULL)
  {
    weight += MSTHead->weight;
    MSTHead = MSTHead->next;
  } 
	return weight;
}

void freeList(AdjPoint listHead){
	//Use recursion to free the list.
	while (listHead != NULL)
  {
    AdjPoint next = listHead->next;
		free(listHead);
    listHead = next;
	}
}

