typedef struct edgeList{
	int firstnode;//The first node in the connection.
	int secondnode;//The second node in the connection.
	int weight;//The weight of the connection.
	struct edgeList *next; //The next node in the linked list.
} AdjRecord, *AdjPoint;

AdjPoint getList(int input);
AdjPoint getMST(AdjPoint listHead);
AdjPoint addEmptyAdjPointToFront(AdjPoint listHead);
AdjPoint getNextEdge(AdjPoint listHead, AdjPoint MSTHead);
AdjPoint AdjCopy(AdjPoint adj);
int sumWeights(AdjPoint MSTHead);
void printMST(AdjPoint MSTHead, int MST);
void printList(AdjPoint listHead);
void freeList(AdjPoint listHead);
int canBeAdded(int firstnode, int secondnode, AdjPoint MSTHead);
int contains(int node, AdjPoint MSTHead);
