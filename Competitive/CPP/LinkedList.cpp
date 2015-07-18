/*
  Author: Joshua Snider
  NOTE: These are all extremely functional and very C-ish,
    they are not a good example of idiomatic C++.

  See HackerRank -> Data Structures -> Linked List.
  Print elements of a linked list on console 
  head pointer input could be NULL as well for empty list
  Node is defined as 
  struct Node
  {
     int data;
     struct Node *next;
  }
*/

/*
* Remove node at given position.
*/
Node*
Delete(Node *head, int position)
{
  if (position == 0) {
    return head->next;
  } else {
    head->next = Delete(head->next, position - 1);
    return head;
  }
}

/*
* Add to end of linked list.
*/
Node*
Insert(Node *head, int data)
{
  if (head == nullptr) {
    Node *end = (Node*)malloc(sizeof(Node));
    end->next = nullptr;
    end->data = data;
    return end;
  } else {
    head->next = Insert(head->next, data);
    return head;
  }
}

/*
* Add at given position of linked list.
*/
Node*
InsertNth(Node *head, int data, int position)
{
  if (position <= 0) {
    Node *ins = (Node*)malloc(sizeof(Node));
    ins->data = data;
    ins->next = head;
    return ins;
  } else {
    head->next = InsertNth(head->next, data, position - 1);
    return head;
  }
}

/*
* Add to start of linked list.
*/
Node*
Prepend(Node *head,int data)
{
  Node *newhead = (Node*)malloc(sizeof(Node));
  newhead->next=head;
  newhead->data = data;
  return newhead;
}

/*
* Print a linked list.
*/
void
Print(Node *head)
{
  if (head != nullptr) {
    std::cout << head->data << std::endl;
    Print(head->next);
  }
}

/*
* Print a linked list in reverse.
*/
void
ReversePrint(Node *head)
{
  if (head != nullptr) {
    ReversePrint(head->next);
    std::cout << head->data << std::endl;
  }
}
